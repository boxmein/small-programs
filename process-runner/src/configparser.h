#ifndef PROCRUNNER_CONFIG_PARSER_H_
#define PROCRUNNER_CONFIG_PARSER_H_
#include <string>
#include <vector>
#include <map>
#include <yaml-cpp/yaml.h>

namespace ProcessRunner {


struct RunnerService {
  std::string type;
  std::string restart;
  std::string cd;
};

struct PrebootStep {
  std::string run;
};

struct SoftwareCheck {
  std::string name;
  std::string version;
};

struct RunnerConfig {
  std::string version;
  std::map<std::string, RunnerService> services;
  std::vector<PrebootStep> preboot;
  std::vector<SoftwareCheck> checkSoftware;
};

RunnerConfig loadConfig(std::string configFileName);

}

namespace YAML {

using SoftwareCheck = ProcessRunner::SoftwareCheck;
using PrebootStep = ProcessRunner::PrebootStep;
using RunnerService = ProcessRunner::RunnerService;
using RunnerConfig = ProcessRunner::RunnerConfig;

template<>
struct convert<SoftwareCheck> {
  static Node encode(const SoftwareCheck& rhs) {
    Node node;

    node["name"] = rhs.name;
    node["version"] = rhs.version;
    return node;
  }

  static bool decode(const Node& node, SoftwareCheck& sc) {
    sc.name = node["name"].as<std::string>();
    sc.version = node["version"].as<std::string>();
    return true;
  }
};

template<>
struct convert<PrebootStep> {
  static Node encode(const PrebootStep& rhs) {
    Node node;
    node["run"] = rhs.run;
    return node;
  }

  static bool decode(const Node& node, PrebootStep& sc) {
    sc.run = node["run"].as<std::string>();
    return true;
  }
};

template<>
struct convert<RunnerService> {
  static Node encode(const RunnerService& rhs) {
    Node node;
    node["type"] = rhs.type;
    node["restart"] = rhs.restart;
    node["cd"] = rhs.cd;
    return node;
  }

  static bool decode(const Node& node, RunnerService& sc) {
    sc.type = node["type"].as<std::string>();
    sc.restart = node["restart"].as<std::string>();
    if (node["cd"]) {
      sc.cd = node["cd"].as<std::string>();
    }
    return true;
  }
};



template<>
struct convert<RunnerConfig> {
  static Node encode(const RunnerConfig& rhs) {
    Node node;
    return node;
  }

  static bool decode(const Node& node, RunnerConfig& rhs) {
    rhs.version = node["version"].as<std::string>();
    for (auto const& service: node["services"]) {
      rhs.services[service.first.as<std::string>()] = (service.second.as<RunnerService>());
    }
    for (auto const& prebootStep: node["preboot"]) {
      rhs.preboot.push_back(prebootStep.as<PrebootStep>());
    }
    for (auto const& softwareCheck: node["check_software"]) {
      rhs.checkSoftware.push_back(softwareCheck.as<SoftwareCheck>());
    }
    return true;
  }
};

}

#endif
