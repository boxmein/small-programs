#include <iostream>
#include <unistd.h>

constexpr int LINE_BUFFER_SIZE = 4096;

void runShellCommand(char* const command) {
  char *const commandLine[] = {
    "/bin/bash",
    "-c",
    command,
    nullptr
  };

  if (!fork()) {
    execvp("/bin/bash", commandLine);
  }
}

int main(int argc, char* argv[]) {
  char lineBuffer[LINE_BUFFER_SIZE];

  if (argc != 2) {
    std::cout << "error: please specify the command in one command line argument, like this: \n"
              << "       ./foreach \"command goes here\"" << std::endl;
  }

  while (!std::cin.eof()) {
    std::cin.getline(lineBuffer, LINE_BUFFER_SIZE);
    runShellCommand(argv[1]);
  }
}

