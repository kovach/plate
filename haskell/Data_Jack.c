#include <jack/jack.h>
#include <jack/transport.h>
typedef jack_default_audio_sample_t sample_t;

typedef struct buffer_list buffer_list;
struct buffer_list {
  sample_t* buffer;
  struct buffer_list* next;
};

