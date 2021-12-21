"""Solutions on day 21.

https://adventofcode.com/2021/day/21
"""
from collections import namedtuple
from pathlib import Path
from typing import List, Optional, Tuple, Union

import click


class DiracDiceDeterministic:
    """Implement the game dirac dice with deterministic die."""
    def __init__(self, start_positions: List[int],
                 goal: int = 1000, n_sides: int = 100):
        """Initialize dirac dice game."""
        self.start_positions = tuple(
            pos - 1
            for pos in start_positions
        )
        self.goal = goal
        self.n_sides = n_sides

    def outcome(self) -> Tuple[int, List[int]]:
        """Compute outcome of the game.

        :returns: Number of dice roled and the final score for each player.
        """
        ns = []
        scores = []
        player_idx = -1
        n_rounds = None
        n_max = None
        for player in range(len(self.start_positions)):
            n, score = self._rounds_to_win(player, n_max)
            ns.append(n)
            scores.append(score)
            if n_rounds is None or n < n_rounds - 1:
                n_rounds = n
                player_idx = player
                n_max = n - 1

        n_dice = 0
        for player, n in enumerate(ns):
            n_played = n_rounds - (player_idx < player)
            n_dice += 3*n_played
            if n > n_played:
                scores[player] = self._score_at_round(player, n_played)

        return n_dice, scores

    def _rounds_to_win(
            self,
            player: int,
            max_rounds: Optional[int] = None
    ) -> Tuple[int, int]:
        """Compute rounds for player to win.

        If max rounds is specified, abort after that round
        and report corresponding score.

        :param player: player index
        :param max_rounds: maximum numbers of rounds to play
        :returns: number of rounds to win (or max_rounds) and the final score
        """
        pos = self.start_positions[player]
        offset = 1 + player*3
        res = 0
        step = len(self.start_positions)*3
        counter = 0
        while True:
            increment = 3*(offset + 1)
            if offset == self.n_sides - 1:
                increment = 2*self.n_sides
            elif offset == 0:
                increment = self.n_sides + 3

            pos = (pos + increment) % 10
            res += pos + 1
            counter += 1
            offset = (offset + step) % self.n_sides

            if res >= self.goal or (max_rounds is not None and
                                    counter >= max_rounds):
                break

        return counter, res

    def _score_at_round(self, player: int, n_rounds: int) -> int:
        """Compute the score of a player after specified round.

        :param player: player index
        :param n_rounds: number of rounds
        :returns: score
        """
        pos = self.start_positions[player]
        offset = 1 + player*3
        res = 0
        step = len(self.start_positions)*3

        for _ in range(n_rounds):
            increment = 3*(offset + 1)
            if offset == self.n_sides - 1:
                increment = 2*self.n_sides
            elif offset == 0:
                increment = self.n_sides + 3
            pos = (pos + increment) % 10
            res += pos + 1
            offset = (offset + step) % self.n_sides

        return res


GameState = namedtuple(
    'GameState',
    [
        'active',  # which player is active
        'positions',  # player positions
        'scores',  # player scores
        'n_universes',  # n universes with this state
    ]
)

# Sum of 3 sided dice roll and number of occurences
ROLLS = {
    3: 1,
    4: 3,
    5: 6,
    6: 7,
    7: 6,
    8: 3,
    9: 1

}


class DiracDiceQuantum:
    """Implement the game dirac dice with quantum die."""
    def __init__(self, start_positions: List[int],
                 goal: int = 21, n_sides: int = 3):
        """Initialize game."""
        self.start_positions = tuple(
            pos - 1
            for pos in start_positions
        )
        self.goal = goal
        self.n_sides = n_sides

    def outcome(self) -> Tuple[int, int]:
        """Compute outcome in all universes.

        Caution: Not the fastest solution. On my machine
        it ran in approximately 35 seconds.

        :returns: Tuples with number of winning universes for player
        1 and player 2.
        """
        wins = [0]*len(self.start_positions)

        stack = [
            GameState(
                active=0,
                positions=self.start_positions,
                scores=tuple([0]*len(self.start_positions)),
                n_universes=1)
        ]

        while len(stack):
            state = stack.pop()
            player = state.active
            old_pos = state.positions[player]
            old_score = state.scores[player]

            for roll, n in ROLLS.items():
                pos = (old_pos + roll) % 10
                score = old_score + pos + 1

                if score >= self.goal:
                    wins[player] += state.n_universes*n
                    continue

                positions = tuple([
                    (1 - player)*pos + player*state.positions[0],
                    player*pos + (1 - player)*state.positions[1]
                ])
                scores = tuple([
                    (1 - player)*score + player*state.scores[0],
                    player*score + (1 - player)*state.scores[1]
                ])
                new_state = GameState(
                    active=(1 - player),
                    positions=positions,
                    scores=scores,
                    n_universes=state.n_universes*n
                )
                stack.append(new_state)

        return wins


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 21 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    with open(path, 'r') as f:
        start_positions = [
            int(line.strip().split()[-1])
            for line in f.readlines()
        ]

    print(f'{len(start_positions)}')
    print(f'{start_positions}')

    print('\nTask 01')
    game = DiracDiceDeterministic(start_positions)
    n_dice, scores = game.outcome()
    print(f'{n_dice=}')
    print(f'{scores=}')
    solution = n_dice * min(scores)
    print(f'{solution=}')

    print('\nTask 02')
    game_q = DiracDiceQuantum(start_positions)
    wins = game_q.outcome()
    # print(f'{wins=}')
    n_winning_universes = max(wins)
    print(f'{n_winning_universes=}')


if __name__ == '__main__':
    main()
