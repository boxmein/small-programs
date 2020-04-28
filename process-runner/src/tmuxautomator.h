#ifndef PROCESSRUNNER_TMUX_AUTOMATOR_H_
#define PROCESSRUNNER_TMUX_AUTOMATOR_H_

#include <string>

namespace ProcessRunner {


class TmuxAutomator {
  private:
    std::string sessionName;

  public:
    explicit TmuxAutomator(std::string sessionName);
    void startInBackground();
    void startNewCommand(std::string tabName, std::string command, std::string workingDirectory);
    void bringToForeground();
};


}

#endif
