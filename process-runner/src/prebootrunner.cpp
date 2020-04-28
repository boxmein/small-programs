#include <cstdlib>
#include <fmt/format.h>
#include "prebootrunner.h"

using namespace ProcessRunner;

void ProcessRunner::runPrebootStep(PrebootStep preboot) {
  fmt::print("Running preboot step\n");
  system(preboot.run.c_str());
}
