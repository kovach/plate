#include <jack/jack.h>
#include <jack/transport.h>

#include "Data_Jack.c"

// global state
double freq;
unsigned long sr;

sample_t* wave;
jack_nframes_t tone_length, wave_length;

sample_t* buffers[22];
int num_buffers;

// build wave table
void build_wave() {
  int i;
	wave_length = sr / freq;

  free(wave);
	wave = (sample_t *) malloc (wave_length * sizeof(sample_t));
  for (i = 0; i < wave_length/2; i++) {
    wave[i] = 2 * (double)i / wave_length;
  }
  for (i = wave_length/2; i < wave_length; i++) {
    wave[i] = 2 - 2 * (double)i / wave_length;
  }
}

int new_buffer(int samples) {
  int i;
  buffers[num_buffers] = (sample_t *) malloc (samples * sizeof(sample_t));
  for (i = 0; i < samples; i++) {
    buffers[num_buffers] = 0;
  }
  num_buffers++;
  return num_buffers;
}

void store_wave(int len, float* w) {
  free(wave);
  wave = w;
  wave_length = len;
}

int get_sr() {
  return sr;
}

void set_freq(double f) {
  freq = f;
  build_wave();
}

double get_freq() {
  return freq;
}

// expects sample rate
void state_init () {
  freq = 200;
  wave = NULL;
  num_buffers = 0;
  build_wave();
}

void state_close() {
  int i;
  for (i = 0; i < num_buffers; i++) {
    free(buffers[i]);
  }
  free(wave);
}

void print_test()
{
  fprintf(stderr, "HELLO\n");
}

void print_ptr (int length, int* array) {
  int i;
  for (i = 0; i < length; i++) {
    printf("%i\n", array[i]);
  }
}
