#include <fmt/format.h>
#include "configparser.h"
#include "prebootrunner.h"

using namespace ProcessRunner;

int main() {
  fmt::print("hello!\n");

  auto config = loadConfig("../examples/a.yml");

  if (config.version != "processrunner.1") {
    fmt::print("Config version invalid: 'processrunner.1' supported, got {}", config.version);
    exit(1);
  }

  for (auto const& prebootStep: config.preboot) {
    fmt::print("Found preboot step\n");
    if (!prebootStep.run.empty()) {
      fmt::print("Found run step in preboot: {}\n", prebootStep.run);
    }
  }

  for (auto const& softwareCheck: config.checkSoftware) {
    if (softwareCheck.name.empty() ||
        softwareCheck.version.empty()) {
      fmt::print("Invalid software check\n");
    }

    fmt::print("Valid software check: {} (version {})\n",
        softwareCheck.name,
        softwareCheck.version
    );
  }

  for (auto const& service: config.services) {
    fmt::print("Found service: {}\n", service.first);
  }
}
