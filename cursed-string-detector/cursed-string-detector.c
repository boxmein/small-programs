#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

#define MAX(a, b) ((a > b) ? (a) : (b))
#define MIN(a, b) ((a < b) ? (a) : (b))

const size_t BUF_SIZE = 32768;

int it_is_time_to_go_bye_bye = 0;

struct mapping {
  size_t start;
  size_t end;
};

struct maps {
  struct mapping *mapping;
  int mappingc;
};

void usage(const char *argv1) {
  printf("usage: %s <pid> <str>", argv1);
}

void sigint() {
  printf("exiting\n");
  it_is_time_to_go_bye_bye = 1;
}

struct maps* getmaps(pid_t pid) {
  char mapsfile[32];
  struct maps* data = malloc(sizeof(struct mapping));
  struct mapping* maplist = calloc(sizeof(struct mapping), 10);
  int idx = 0;

  if (kill(pid, 0) == -1) {
    perror("process ID invalid");
    if (data != NULL) {
      free(data);
    }
    if (maplist != NULL) {
      free(maplist);
    }
    return NULL;
  }

  sprintf(mapsfile, "/proc/%d/maps", pid);
  FILE* mapsfd = fopen(mapsfile, "rb");

  if (data == NULL) {
    perror("error allocating");
    if (maplist != NULL) {
      free(maplist);
    }
    return NULL;
  }
  
  if (maplist == NULL) {
    perror("error allocating");
    if (data != NULL) {
      free(data);
    }
    return NULL;
  }

  if (mapsfd == NULL) {
    perror("error opening maps file");
    free(maplist);
    return NULL;
  }

  assert(mapsfd != NULL);

  char line[512];
  while (fgets(line, 512, mapsfd) != NULL) {
    if (idx > 9) {
      break;
    }

    assert(strlen(line) > 0);

    if (!strstr(line, "[stack]") && !strstr(line, "[heap]")) {
      continue;
    }

    size_t left;
    size_t right;
    sscanf(line, "%lx-%lx", &left, &right);
    maplist[idx].start = left;
    maplist[idx].end = right;
    printf("region %lx-%lx found\n", left, right);
    idx += 1;
  }

  fclose(mapsfd);
  
  assert(data != NULL);
  data->mapping = maplist;
  data->mappingc = idx;

  return data;
}

const char * whereis(const char *haystack, const size_t haystack_size, const char* needle) {
  size_t needle_len = strlen(needle);
  size_t max_left_pos = (size_t) haystack + haystack_size - needle_len;

  for (size_t i = (size_t) haystack; i < max_left_pos; i ++) {
    const char* new_start = (const char*) i;
    if (memcmp(new_start, needle, needle_len) == 0) {
      return new_start;
    }
  }
  return NULL;
}

const char* scanregion(int proc_mem_fd, size_t start, size_t end, const char* scan) {
  const char* found = NULL;
  int rc = 1;

  int size = end - start;
  char *buf = malloc(size);

  if (buf == NULL) {
    perror("malloc scanregion failed");
    return NULL;
  }

  memset(buf, 0, size);

  assert(proc_mem_fd != NULL);
  assert(start > 0);
  assert(start < end);
  assert(size > 0);

  if (!lseek(proc_mem_fd, start, 0)) {
    perror("failed to lseek()");
    free(buf);
    return NULL;
  } 

  rc = read(proc_mem_fd, buf, size);
  if (rc != size) {
    perror("scanner read memory");
    free(buf);
    return NULL;
  }
  found = whereis(buf, size, scan);
  if (found != NULL) {
    printf("\rWOW found it at %p\n", found); 
    free(buf);
    return NULL;
  }

  free(buf);
  return found;
}

int getprocmem(pid_t pid) {
  char buf[32];
  if (kill(pid, 0) == -1) {
    perror("process ID invalid");
  }

  sprintf(buf, "/proc/%d/mem", pid);
  int proc_mem_fd = open(buf, O_RDONLY);

  if (proc_mem_fd == NULL) {
    perror("error opening mem");
    return NULL;
  }
  return proc_mem_fd;
}

const char* scanner(int proc_mem_fd, struct maps* maps, const char *scan) {
  const char* found = NULL;
  assert(proc_mem_fd != NULL);
  assert(maps != NULL);
  assert(scan != NULL);
  for (int i = 0; i < maps->mappingc; i++) {
    assert(maps->mapping != NULL);
    size_t start = maps->mapping[i].start;
    size_t end = maps->mapping[i].end;
    assert(start > 0);
    assert(end > 0);
    found = scanregion(proc_mem_fd, start, end, scan);
    if (found != NULL) {
      break;
    }
  }
  return found;
}

int horrible(const pid_t pid, const char *scan) {
  if (ptrace(PTRACE_ATTACH, pid, NULL, NULL) == -1) {
    fprintf(stderr, "pid %d attach failure: ");
    perror("ptrace(PTRACE_ATTACH) failed");
  }
  
  int proc_mem_fd = getprocmem(pid);

  if (proc_mem_fd == NULL) {
    return 1;
  }

  struct maps* maps = getmaps(pid);

  if (maps == NULL) {
    printf("reading maps failure\n");
    return 1;
  }
  assert(kill(pid, 0) != -1);
  assert(maps != NULL);
  assert(scan != NULL);
  scanner(proc_mem_fd, maps, scan);


  int status = 0;
  while(1) {
    if (it_is_time_to_go_bye_bye) {
      break;
    }
    if (ptrace(PTRACE_SINGLESTEP, pid) == -1) {
      fprintf(stderr, "error stepping on %d:", pid);
      perror("ptrace(PTRACE_SINGLESTEP) failed");
      break;
    }
    wait(&status);
    if (WIFEXITED(status)) {
      fprintf(stderr, "pid %d WIFEXITED %d\n", pid, status);
      break;
    }
    scanner(proc_mem_fd, maps, scan);
  }

  if (ptrace(PTRACE_DETACH, pid, NULL, NULL) == -1) {
    fprintf(stderr, "pid %d: ");
    perror("ptrace(PTRACE_DETACH) failed");
  }
  assert(proc_mem_fd != NULL);
  close(proc_mem_fd);
  free(maps);
}

int main(int argc, char *argv[]) {
  if (argc < 3) {
    usage(argv[0]);
    return 1;
  }

  signal(SIGINT, sigint);

  pid_t pid = (pid_t) atoi(argv[1]);
  const char* scan = argv[2];

  printf("Scanning for '%s' in process %d\n", scan, pid);

  assert(pid > 1);
  assert(strlen(scan) > 0);
  assert(strlen(scan) < BUF_SIZE);

  horrible(pid, scan);
}