#!/usr/bin/env python3
# encoding: utf-8
import asyncio
import json
import locale
import logging
import random
import string
import sys
import time
from dataclasses import dataclass
from typing import NewType, Protocol, Sequence

import requests

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
locale.setlocale(locale.LC_ALL, '')
code = locale.getpreferredencoding()

Flag = NewType("Flag", str)
Team = NewType("Team", str)
FlagstoreName = NewType("FlagstoreName", str)

FlagsCapturedBy = dict[Team, list[Flag]]
FlagsCapturedPerFlagstorePerTeam = dict[FlagstoreName, dict[Team, list[Flag]]]
FlagsOwnedPerFlagstorePerTeam = dict[FlagstoreName, dict[Team, list[Flag]]]
AllCapturesOf = dict[Flag, list[Team]]
FlagsOwnedBy = dict[Team, list[Flag]]

class Flagstore(Protocol):
    def name(self):
        ...
    def add_flag(self, team: Team, flag: Flag):
        ...
    def get_flags(self) -> list[Flag]:
        ... 


class NopFlagstore(Flagstore):
    _name: str
    _flags: list[Flag] = []
    def __init__(self, name: str):
        self._name = name

    def name(self):
        return self._name

    def get_flags(self):
        return self._flags[:-5]

    def add_flag(self, team: Team, flag: Flag):
        self._flags.append(flag)

class AttackDefenseCTF:
    teams: Sequence[Team]
    flagstores: Sequence[Flagstore]
    flag_to_flagstore: dict[Flag, Flagstore] = {}

    _current_tick = 0
    max_ticks: int

    flags_captured_by: FlagsCapturedBy = {}
    flags_captured_per_flagstore: FlagsCapturedPerFlagstorePerTeam = {}
    flags_owned_by: FlagsOwnedBy = {}
    flags_owned_per_flagstore: FlagsOwnedPerFlagstorePerTeam = {}
    all_captures_of: AllCapturesOf = {}

    def __init__(self, teams: Sequence[Team], flagstores: Sequence[Flagstore], max_ticks: int):
        self.teams = teams
        self.flagstores = flagstores
        self.max_ticks = max_ticks

    def get_total_offense_points(self, team: Team) -> float:
        if team not in self.flags_captured_by:
            return 0
        assert team in self.flags_captured_by
        # simple: 1 point per flag stolen
        offense: float = len(self.flags_captured_by[team])
        # exponential: extra points for rare flags
        for flag in self.flags_captured_by[team]:
            assert flag in self.all_captures_of
            offense += (1 / len(self.all_captures_of[flag]))
        return offense

    def get_total_defense_points(self, team: Team) -> float:
        if team not in self.flags_owned_by:
            return 0
        # -N ** 0.75 for N flags stolen
        assert team in self.flags_owned_by
        defense = 0
        for flag in self.flags_owned_by[team]:
            if flag not in self.all_captures_of:
                continue
            defense -= len(self.all_captures_of[flag]) ** 0.75
        return defense
        
    def get_defense_points_per_flagstore(self, team: Team, flagstore: Flagstore) -> float:
        flagstore_name = flagstore.name()
        
        if flagstore_name not in self.flags_owned_per_flagstore:
            return 0
        assert flagstore_name in self.flags_owned_per_flagstore
        
        if team not in self.flags_owned_per_flagstore[flagstore_name]:
            return 0
        assert team in self.flags_owned_per_flagstore[flagstore_name]

        defense = 0.0
        for flag in self.flags_owned_per_flagstore[flagstore_name][team]:
            if flag not in self.all_captures_of:
                continue
            defense -= len(self.all_captures_of[flag]) ** 0.75
        
        return defense

    def get_offense_points_per_flagstore(self, team: Team, flagstore: Flagstore) -> float:
        flagstore_name = flagstore.name()

        if flagstore_name not in self.flags_captured_per_flagstore:
            return 0
        assert flagstore_name in self.flags_captured_per_flagstore

        if team not in self.flags_captured_per_flagstore[flagstore_name]:
            return 0
        assert team in self.flags_captured_per_flagstore[flagstore_name]

        offense = float(len(self.flags_captured_per_flagstore[flagstore_name][team]))
        for flag in self.flags_captured_per_flagstore[flagstore_name][team]:
            assert flag in self.all_captures_of 
            offense += (1 / len(self.all_captures_of[flag]))
        return offense
        
    def is_flag_stolen_by_team(self, team: Team, flag: Flag) -> bool:
        if team not in self.flags_captured_by:
            return False
        return flag in self.flags_captured_by[team]

    def add_captured_flag(self, team: Team, flag: Flag) -> bool: 
        if team not in self.flags_captured_by:
            self.flags_captured_by[team] = []
        # don't double steal flags
        assert flag not in self.flags_captured_by[team]
        self.flags_captured_by[team].append(flag)
        return True
    
    def add_team_to_flag(self, team: Team, flag: Flag) -> bool:
        if flag not in self.all_captures_of:
            self.all_captures_of[flag] = []
        assert team not in self.all_captures_of[flag]
        self.all_captures_of[flag].append(team)
        return True

    def add_owned_flag(self, team: Team, flag: Flag):
        if team not in self.flags_owned_by:
            self.flags_owned_by[team] = []
        self.flags_owned_by[team].append(flag)

    def add_owned_by_flagstore_flag(self, team: Team, flag: Flag, flagstore: Flagstore):
        flagstore_name = flagstore.name()
        if flagstore_name not in self.flags_owned_per_flagstore:
            self.flags_owned_per_flagstore[flagstore_name] = {}
        if team not in self.flags_owned_per_flagstore[flagstore_name]:
            self.flags_owned_per_flagstore[flagstore_name][team] = []
        
        self.flags_owned_per_flagstore[flagstore_name][team].append(flag)

    def add_flag_to_flagstore(self, flag: Flag, flagstore: Flagstore):
        self.flag_to_flagstore[flag] = flagstore

    def add_captured_by_flagstore_flag(self, team: Team, flag: Flag, flagstore: Flagstore):
        flagstore_name = flagstore.name()
        if flagstore_name not in self.flags_captured_per_flagstore:
            self.flags_captured_per_flagstore[flagstore_name] = {}
        if team not in self.flags_captured_per_flagstore[flagstore_name]:
            self.flags_captured_per_flagstore[flagstore_name][team] = []
        self.flags_captured_per_flagstore[flagstore_name][team].append(flag)


    def do_steal_flag(self, team: Team, flag: Flag, flagstore: Flagstore):
        if self.is_flag_stolen_by_team(team, flag):
            return

        self.add_captured_flag(team, flag)
        self.add_team_to_flag(team, flag)
        self.add_captured_by_flagstore_flag(team, flag, flagstore)
        logging.debug(f"flag stolen: {team} {flag}")

    def do_new_flag(self, team: Team, flag: Flag, flagstore: Flagstore):
        self.add_owned_flag(team, flag)
        self.add_flag_to_flagstore(flag, flagstore)
        self.add_owned_by_flagstore_flag(team, flag, flagstore)
        logging.debug(f"flag generated: {team} {flag}")

    def generate_flag(self, flagstore: Flagstore) -> Flag: 
        randomstuff = ''.join(random.sample(string.ascii_lowercase, 12))
        flag = Flag(f"flag-{flagstore.name()}-{randomstuff}")
        return flag

    def run_tick(self):
        for team in self.teams:
            for flagstore in self.flagstores:
                flag = self.generate_flag(flagstore)
                flagstore.add_flag(team, flag)
                self.do_new_flag(team, flag, flagstore)

class TeamSimulator(Protocol):
    def simulate_tick(self): ...

class GoodTeamSimulator(TeamSimulator):
    team: Team
    teamk: float = 0.1
    ctf: AttackDefenseCTF

    _progression = 0

    def __init__(self, team: Team, ctf: AttackDefenseCTF):
        self.team = team
        self.ctf = ctf 
    
    def simulate_tick(self):
        self._progression += 1
        logging.debug(f"{self.team}:simulate_tick({self._progression})")
        flagstores_owned = self.get_flagstores_owned()
        for flagstore in flagstores_owned:
            received_flags = flagstore.get_flags()
            for flag in received_flags:
                logging.debug(f"{self.team}:stealing_flag:{flag}")
                self.ctf.do_steal_flag(self.team, flag, flagstore)
        

    def get_flagstores_owned(self) -> Sequence[Flagstore]:
        all_flagstores = self.ctf.flagstores
        percent_ticks_done = self._progression / self.ctf.max_ticks
        percent_flagstores_owned = percent_ticks_done + self.teamk
        left_idx = 0
        right_idx = int(len(all_flagstores) * percent_flagstores_owned)

        logging.debug(f"{self.team}:flagstores_pwned:{right_idx}")
        return all_flagstores[left_idx:right_idx]


@dataclass
class Cell:
    team: Team
    flagstore: Flagstore
    offense_points: float
    defense_points: float
    sla_points: float

    def get_total_score(self):
        return self.offense_points + self.defense_points + self.sla_points

@dataclass
class TeamScore:
    team: Team
    offense_points: float 
    defense_points: float 
    sla_points: float 

@dataclass
class DownloadedState:
    pass

class ScoreMatrix:
    ctf: AttackDefenseCTF
    team_scores: dict[Team, TeamScore]
    team_services: dict[Team, dict[FlagstoreName, Cell]]

    def __init__(self, ctf: AttackDefenseCTF):
        self.ctf = ctf 
        self.team_scores = self.get_team_scores()
        self.cells = self.get_cells()

    def get_team_scores(self) -> dict[Team, TeamScore]:
        result = {}
        for team in self.ctf.teams:
            teamscore = TeamScore(
                team,
                self.ctf.get_total_offense_points(team),
                self.ctf.get_total_defense_points(team),
                0
            )
            result[team] = teamscore

        return result

    def get_cells(self) -> dict[Team, dict[FlagstoreName, Cell]]:
        rows_by_team: dict[Team, dict[FlagstoreName, Cell]] = {}
        for team in self.ctf.teams:
            row: dict[FlagstoreName, Cell] = {}
            for flagstore in self.ctf.flagstores:
                row[flagstore] = self.get_cell(team, flagstore)
            rows_by_team[team] = row

        return rows_by_team

    def get_cell(self, team: Team, flagstore: Flagstore) -> Cell:
        return Cell(
            team,
            flagstore, 
            self.ctf.get_offense_points_per_flagstore(team, flagstore),
            self.ctf.get_defense_points_per_flagstore(team, flagstore),
            0
        )

teams: Sequence[Team] = [
    Team("Austria"),
    Team("Belgium"),
    Team("Croatia"),
    Team("Cyprus"),
    Team("Czechia"),
    Team("Denmark"),
    Team("Estonia"),
    Team("Finland"),
    Team("France"),
    Team("Germany"),
    Team("Greece"),
    Team("Hungary"),
    Team("Iceland"),
    Team("Ireland"),
    Team("Italy"),
    Team("Liechtenstein"),
    Team("Luxembourg"),
    Team("Malta"),
    Team("Netherlands"),
    Team("Norway"),
    Team("Poland"),
    Team("Portugal"),
    Team("Romania"),
    Team("Slovakia"),
    Team("Slovenia"),
    Team("Spain"),
    Team("Sweden"),
    Team("Switzerland"),
    Team("G-Canada"),
    Team("G-Israel"),
    Team("G-Serbia"),
    Team("G-USA"),
    Team("G-UAE"),
]

flagstores: Sequence[Flagstore] = [
    NopFlagstore("Dewaste-1"),
    NopFlagstore("Cantina-1"),
    NopFlagstore("HPS-1"),
    NopFlagstore("Aquaeductus-1"),
    NopFlagstore("Blinkygram-1"),
    NopFlagstore("Winds-of-the-Past-1"),
    NopFlagstore("Techbay-1"),
]

def run_tick(simulators: Sequence[TeamSimulator]):
    for simulator in simulators:
        simulator.simulate_tick()

def extract_scores(ctf: AttackDefenseCTF) -> ScoreMatrix:
    return ScoreMatrix(ctf)

def print_scores(matrix: ScoreMatrix):
    for team, scores in matrix.team_scores.items():
        print(f"{team:>20} {scores.offense_points:>10.4f} {scores.defense_points:>10.4f} {scores.sla_points:>10.4f}")
    # print(matrix.team_scores)
    print(matrix.cells)

def download_teams():
    teams = requests.get('https://scoreboard.ecsc.at/scoreboard_teams.json').json()

    return [
        Team(item["name"])
        for item in teams
    ]

def download_state():
    requests.get('https://scoreboard.ecsc.at/scoreboard_teams.json')

@dataclass
class CurrentData:
    current_tick: int 
    current_tick_until: int 
    scoreboard_tick: int 
    state: int

def download_current():
    return CurrentData(**requests.get('https://scoreboard.ecsc.at/scoreboard_current.json').json())

def get_teams():
    try:
        with open('./teams.json', 'r') as fd:
            return json.load(fd)
    except:
        teams = download_teams()
        with open('./teams.json', 'w') as fd:
            fd.write(json.dumps(teams))
        return teams

@dataclass 
class RoundScoreboardService:
    """ offensive points """
    o: float 
    """ defensive points """
    d: float 
    """ sla points """
    s: float 
    """ flags gained over all ticks """
    st: float 
    """ flags lost over all ticks """
    cap: float 

    """ offensive change since last tick """ 
    do: float 
    """ defensive change since last tick """
    dd: float 
    """ sla change since last tick """
    ds: float
    """ flags gained since last tick """ 
    dst: float
    """ flags lost since last tick """
    dcap: float 

    """ message """ 
    m: str 

    """ uptime state (definitions can be loaded from the same JSON object's .status-descriptions) """
    c: int 
    """ last 3 uptime states """
    dc: list[int]

@dataclass
class RoundScoreboard:
    team_id: int 
    """ offensive points (total) """
    o: float 
    """ defensive points (total) """
    d: float
    """ sla points (total) """
    s: float 

    """ offensive points added (total) """
    do: float 
    """ defensive points added (total) """
    dd: float
    """ sla points added (total) """
    ds: float 

    """ above numbers, per-service """
    services: list[RoundScoreboardService]


@dataclass 
class RoundService:
    name: str 
    """ team ids with first-blood """
    first_blood: list[int]

    """ count of teams gaining flags """
    attackers: int 
    """ count of teams losing flags """
    victims: int 


@dataclass 
class RoundData:
    tick: int 
    services: list[RoundService]
    scoreboard: list[RoundScoreboard]

if __name__ == '__main__':
    print("=== A/D CTF SIMULATOR ===")

    downloaded_state = download_state()

    ctf = AttackDefenseCTF(teams, flagstores, 150)

    all_teams: Sequence[TeamSimulator] = [
        GoodTeamSimulator(team, ctf)
        for team in teams
    ]

    max_tick = 8
    for tick in range(max_tick):
        print(f"\r{tick}/{max_tick}", end="")
        # start tick
        ctf.run_tick()
        # simulate team behavior
        run_tick(all_teams)

    print("\n")
    scores = extract_scores(ctf)
    print_scores(scores)

