#!/usr/bin/env python3
"""Animate Prim's minimum spanning tree algorithm.

Python equivalent of MST_PriGif.m. The script creates random points, draws
their Delaunay triangulation, constructs a complete Euclidean graph, computes
its minimum spanning tree with Prim's algorithm, and saves a looping GIF.

Requirements:
    python -m pip install numpy scipy matplotlib pillow

Example:
    python MST_PriGif.py --seed 123 --output Prim_python.gif
"""

from argparse import ArgumentParser
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation, PillowWriter
from matplotlib.collections import LineCollection
from PIL import Image, ImageSequence
from scipy.spatial import Delaunay
from scipy.spatial.distance import cdist


def prim_mst(distance_matrix, start=0):
    """Return MST edges ``(source, target, weight)`` in Prim selection order."""
    number_of_nodes = distance_matrix.shape[0]
    selected = np.zeros(number_of_nodes, dtype=bool)
    selected[start] = True
    edges = []

    for _ in range(number_of_nodes - 1):
        crossing_distances = distance_matrix.copy()
        crossing_distances[~selected, :] = np.inf
        crossing_distances[:, selected] = np.inf

        source, target = np.unravel_index(
            np.argmin(crossing_distances),
            crossing_distances.shape,
        )
        weight = crossing_distances[source, target]

        if not np.isfinite(weight):
            raise ValueError("The graph is disconnected; no spanning tree exists.")

        edges.append((source, target, weight))
        selected[target] = True

    return np.asarray(edges)


def animation_edge_counts(number_of_edges):
    """Match the progressive reveal schedule used by the MATLAB script."""
    fractions = np.concatenate(
        [
            np.arange(0.01, 0.10, 0.01),
            np.arange(0.10, 1.00, 0.10),
            np.arange(0.91, 1.01, 0.01),
        ]
    )
    counts = [0]
    counts.extend(int(np.floor(number_of_edges * value)) for value in fractions)
    return counts


def make_gif_transparent(input_path, output_path, threshold=245):
    """Make near-white pixels transparent in every frame of an animated GIF."""
    source = Image.open(input_path)
    transparent_frames = []
    durations = []

    for frame in ImageSequence.Iterator(source):
        rgba = frame.convert("RGBA")
        pixels = list(rgba.getdata())
        transparent_mask = [
            red >= threshold and green >= threshold and blue >= threshold
            for red, green, blue, _ in pixels
        ]

        # Reserve palette index 255 for transparency.
        indexed = rgba.convert("RGB").quantize(
            colors=255,
            method=Image.Quantize.MEDIANCUT,
        )
        indices = bytearray(indexed.tobytes())
        for index, is_transparent in enumerate(transparent_mask):
            if is_transparent:
                indices[index] = 255

        transparent_frame = Image.frombytes("P", indexed.size, bytes(indices))
        palette = indexed.getpalette()[:765]
        palette.extend([255, 255, 255])
        transparent_frame.putpalette(palette)
        transparent_frame.info["transparency"] = 255
        transparent_frame.info["disposal"] = 2

        transparent_frames.append(transparent_frame)
        durations.append(
            frame.info.get("duration", source.info.get("duration", 500))
        )

    if not transparent_frames:
        raise ValueError("The generated GIF contains no frames.")

    transparent_frames[0].save(
        output_path,
        save_all=True,
        append_images=transparent_frames[1:],
        duration=durations,
        loop=source.info.get("loop", 0),
        transparency=255,
        disposal=2,
        optimize=False,
    )


def create_animation(number_of_points=150, seed=None, output_path="Prim_python.gif"):
    """Create and save the animated Delaunay graph and Prim MST."""
    rng = np.random.default_rng(seed)
    points = rng.random((number_of_points, 2))

    triangulation = Delaunay(points)
    distance_matrix = cdist(points, points)
    np.fill_diagonal(distance_matrix, np.inf)
    mst_edges = prim_mst(distance_matrix, start=0)

    figure, axis = plt.subplots(figsize=(8, 8), facecolor="none")
    figure.patch.set_alpha(0)
    axis.set_facecolor("none")
    axis.set_xlim(0, 1)
    axis.set_ylim(0, 1)
    axis.set_aspect("equal")
    axis.axis("off")

    axis.triplot(
        points[:, 0],
        points[:, 1],
        triangulation.simplices,
        color=(0.86, 0.86, 0.86),
        linewidth=1,
        zorder=1,
    )
    axis.plot(
        points[:, 0],
        points[:, 1],
        linestyle="none",
        marker="o",
        markeredgecolor="blue",
        markerfacecolor="none",
        markersize=4,
        zorder=3,
    )

    tree_lines = LineCollection([], colors="red", linewidths=3, zorder=2)
    axis.add_collection(tree_lines)

    edge_counts = animation_edge_counts(len(mst_edges))

    def update(frame_index):
        edge_count = edge_counts[frame_index]
        visible_edges = mst_edges[:edge_count, :2].astype(int)
        segments = [points[[source, target]] for source, target in visible_edges]
        tree_lines.set_segments(segments)
        return (tree_lines,)

    animation = FuncAnimation(
        figure,
        update,
        frames=len(edge_counts),
        interval=500,
        blit=True,
        repeat=True,
    )

    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    temporary_path = output_path.with_name(
        f"{output_path.stem}_temporary{output_path.suffix}"
    )

    # Matplotlib 3.10.0 has a PillowWriter bug for transparent RGBA frames.
    # Render an opaque temporary GIF, then add reliable palette transparency.
    figure.patch.set_facecolor("white")
    figure.patch.set_alpha(1)
    axis.set_facecolor("white")
    animation.save(
        temporary_path,
        writer=PillowWriter(fps=2),
        dpi=120,
        savefig_kwargs={"transparent": False, "facecolor": "white"},
    )
    make_gif_transparent(temporary_path, output_path)
    temporary_path.unlink()
    plt.close(figure)

    total_weight = mst_edges[:, 2].sum()
    print(f"Saved animation: {output_path.resolve()}")
    print(f"Points: {number_of_points}")
    print(f"MST edges: {len(mst_edges)}")
    print(f"Total MST weight: {total_weight:.6f}")

    return points, mst_edges


def main():
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        "--points",
        type=int,
        default=150,
        help="number of random points (default: 150)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=None,
        help="optional random seed for reproducible output",
    )
    parser.add_argument(
        "--output",
        default=str(Path(__file__).with_name("Prim_python.gif")),
        help="output GIF path",
    )
    arguments = parser.parse_args()

    if arguments.points < 2:
        parser.error("--points must be at least 2")

    create_animation(
        number_of_points=arguments.points,
        seed=arguments.seed,
        output_path=arguments.output,
    )


if __name__ == "__main__":
    main()
