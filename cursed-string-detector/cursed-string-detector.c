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

  char line[512];
  while (fgets(line, 512, mapsfd) != NULL) {
    if (idx > 9) {
      break;
    }

    if (!strstr(line, "[stack]")) {
      //  && !strstr(line, "[heap]")
      // printf("line did not contain stack or heap: %s", line);
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

const char* scanregion(FILE *fd, size_t start, size_t end, const char* scan) {
  char buf2[BUF_SIZE];
  const char* found = NULL;
  int rc = 1;
  size_t current = start;
  while (current < end) {
    int to_read = MIN(BUF_SIZE, (int) end - (int) current);
    fseek(fd, current, 0); 
    rc = fread(buf2, 1, to_read, fd);
    if (rc != to_read) {
      perror("scanner read memory");
      break;
    }
    found = whereis(buf2, to_read, scan);
    if (found != NULL) {
      printf("\nWOW found it at %p\n", found); 
      break;
    } 
    current = current + to_read;
  }
  return found;
}

FILE * getprocmem(pid_t pid) {
  char buf[32];


  if (kill(pid, 0) == -1) {
    perror("process ID invalid");
  }

  sprintf(buf, "/proc/%d/mem", pid);
  FILE *fd = fopen(buf, "rb");

  if (fd == NULL) {
    perror("error opening mem");
    return NULL;
  }
  return fd;
}

const char* scanner(pid_t pid, struct maps* maps, const char *scan) {
  FILE * fd = getprocmem(pid);
  const char* found = NULL;

  if (fd == NULL) {
    return NULL;
  }
  
  for (int i = 0; i < maps->mappingc; i++) {
    size_t start = maps->mapping[i].start;
    size_t end = maps->mapping[i].end;
    found = scanregion(fd, start, end, scan);
    if (found != NULL) {
      break;
    }
  }
  fclose(fd);
  return found;
}

int horrible(pid_t pid, const char *scan) {
  ptrace(PTRACE_ATTACH, pid, NULL, NULL);

  struct maps* maps = getmaps(pid);

  if (maps == NULL) {
    printf("reading maps failure\n");
    return 1;
  }

  while(1) {  
    if (it_is_time_to_go_bye_bye) {
      return 0;
    }
    scanner(pid, maps, scan);
    ptrace(PTRACE_SINGLESTEP, pid);
    sleep(1);
  }
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
  ptrace(PTRACE_DETACH, pid, NULL, NULL);
}