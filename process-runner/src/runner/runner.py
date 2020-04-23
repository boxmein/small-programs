from .base.configuration import load_configuration

def run_from_conf(conf):
    for name, service in enumerate(conf.services):
        print("starting service", name)
        # service_runner.start(service)

def start():
    conf = load_configuration()
