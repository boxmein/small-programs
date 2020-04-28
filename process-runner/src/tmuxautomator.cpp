#include <fmt/format.h>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "tmuxautomator.h"

using namespace ProcessRunner;

TmuxAutomator::TmuxAutomator(std::string sessionName):
  sessionName(sessionName) {}

void TmuxAutomator::startInBackground() {
  fmt::print("Start tmux in background with a new session name\n");
  // NOTE: shell injection vulnerability
  system(fmt::format("tmux new -s {}", sessionName).c_str());
}

void TmuxAutomator::startNewCommand(
    std::string tabName,
    std::string command,
    std::string workingDirectory
  ) {
  fmt::print("Start command in tmux, with its own window named {}\n", tabName);
  fmt::print("Command is: {}\n", command);

}

void TmuxAutomator::bringToForeground() {
  fmt::print("Bringing tmux to foreground\n");
  pid_t pid = fork();

  if (pid == 0) {
    execlp("tmux", "attach", "-t", "wowza", nullptr);
  } else {
    int exitcode = 0;
    pid_t result = wait(&exitcode);
    if (WIFEXITED(exitcode)) {
      fmt::print("Tmux exited\n");
      fmt::print("Bye bye\n");
    }
  }
}
