#include "configparser.h"
using namespace ProcessRunner;
RunnerConfig ProcessRunner::loadConfig(std::string configFileName) {
  auto document = YAML::LoadFile(configFileName.c_str());
  return document.as<RunnerConfig>();
}
