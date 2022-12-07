from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import click


@dataclass
class Node:
    name: str
    type: str
    size: int = -1
    parent: Optional["Node"] = None
    children: dict[str, "Node"] = field(default_factory=dict)

    @staticmethod
    def from_text(text: str) -> "Node":
        """Parse text input and build tree

        Parameters
        ----------
        text : str
            Text representation of file tree

        Returns
        -------
        Node
            Returns root node
        """
        root = Node("/", "dir")
        node = root

        # Skip first '$ cd /' line
        for line in text.split("\n")[1:]:
            line = line.strip()
            if line.startswith("$"):
                parts = line.split(" ")
                # We have a cd command
                if len(parts) != 3:
                    continue

                dest = parts[-1]
                if dest == "/":
                    node = root
                elif dest == "..":
                    assert node.parent is not None
                    node = node.parent
                else:
                    node = node.children[dest]

                continue

            if line.startswith("dir"):
                name = line.split(" ")[-1]
                node.children[name] = Node(
                    name=name,
                    type="dir",
                    parent=node,
                )
                continue

            if len(line) != 0:
                sz, name = line.split(" ")
                assert sz.isnumeric()
                size = int(sz)
                node.children[name] = Node(
                    name=name,
                    type="file",
                    parent=node,
                    size=size,
                )

        return root

    def print_listing(self) -> str:
        """Generate file listing

        Similar to the example on the webpage. For debugging.

        Returns
        -------
        str
            Nested list of files and directories
        """
        stack = [(self, 0)]
        result = ""

        while len(stack) != 0:
            node, depth = stack.pop()

            result += "  " * depth
            result += f"- {node.name} ({node.type}, size={node.size})\n"

            for _, child in sorted(node.children.items(), reverse=True):
                stack.append((child, depth + 1))

        return result

    def compute_dir_sizes(self, dir_limit: int = 100_000) -> int:
        """Compute the cumulative size of directories and the size of small directories

        Parameters
        ----------
        dir_limit : int, optional
            Limit for a directory to be considered small, by default 100_000

        Returns
        -------
        int
            Total size of all small directories
        """
        stack = [(self, False)]
        result = 0

        while len(stack) != 0:
            node, visited = stack.pop()

            if node.type == "file":
                continue

            if visited:
                node.size = sum(child.size for child in node.children.values())
                if node.size < dir_limit:
                    result += node.size
                continue

            stack.append((node, True))
            for child in node.children.values():
                stack.append((child, False))

        return result

    def find_smallest_deletable_directory(
        self, max_space: int = 70_000_000, min_space: int = 30_000_000
    ) -> int:
        """Find smallest directory to delete to achieve minimal disk space

        Parameters
        ----------
        max_space : int, optional
            Maximum disk space of device, by default 70_000_000
        min_space : int, optional
            Minimum disk space required for update, by default 30_000_000

        Returns
        -------
        int
            Size of smallest directory to delete to achieve min_space
        """
        root_size = self.size
        if root_size == -1:
            self.compute_dir_sizes()
            root_size = self.size

        remaining_space = max_space - root_size
        result = root_size

        stack = [self]

        while len(stack) != 0:
            node = stack.pop()

            if node.type == "file":
                continue

            stack.extend(node.children.values())

            if node.size + remaining_space >= min_space and node.size < result:
                result = node.size

        return result


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 07

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:

        root = Node.from_text(f.read())

    result = root.compute_dir_sizes()
    print(root.print_listing())
    print(f"Task 01: {result}")

    result = root.find_smallest_deletable_directory()
    print(f"Task 02: {result}")


if __name__ == "__main__":
    main()
