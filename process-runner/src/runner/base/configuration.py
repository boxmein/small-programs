#!/usr/bin/env python3
from yaml import load

from .constants import DEFAULT_CONFIGURATION_FILENAME

class Configuration:
    services = {}
    preboot = []
    check_software = []

    def from_dict(definition):
        conf = Configuration()
        for preboot_step in definition['preboot']:
            conf.preboot.append(
                PrebootStep.from_dict(
                    preboot_step
                )
            )
        for name, service in enumerate(definition['services']):
            conf.services[name] = Service.from_dict(name, service)

class Service:
    name = ''
    service_type = ''
    restart_strategy = ''
    directory = ''

    def from_dict(service_name, definition):
        svc = Service()
        svc.name = service_name
        svc.service_type = definition['type']
        svc.restart_strategy = definition['restart']
        svc.directory = definition['cd']
        return svc

class PrebootStep:
    step_type = ''
    run_command = ''

    def from_dict(definition):
        pbs = PrebootStep()
        pbs.step_type = 'run_command'
        pbs.run_command = definition['run']

class SoftwareCheck:
    check_type = ''
    name = ''
    version = ''

def load_configuration():
    print("loading file: ", DEFAULT_CONFIGURATION_FILENAME)

    with open(DEFAULT_CONFIGURATION_FILENAME, 'r') as fd:
        contents = fd.read()
        loaded = load(contents)
        print("loaded yaml", loaded)
        return Configuration.from_dict(contents)
