#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <GLFW/glfw3.h>
#include <OpenGL/GL.h>

#define SZ 2048
#define SZ2 pow(SZ,2)
#define SL 100

typedef struct { double x; double y; double z; } cam;

void draw_map(int16_t *map, int sz) {
  for(int i=0;i<sz;i++) { if((int8_t)(map[i] & 0xff)>SL) {
    glVertex3f((double)(i%SZ)/SZ,i/SZ2,0); } } }
void in_map(FILE *f, int16_t **map, int sz) {
  for(int i=0;i<sz;i++) { fread(&(*map)[i],sizeof(int16_t),1,f); } }

void error_callback(int error, const char* description) {
    fputs(description, stderr); }
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE); }
void rsz(GLFWwindow *win, int w, int h) {
  glViewport(0,0,w,h); float ratio = w/ (float) h;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, 1, 0, 1.f, 1.f, 0);
  glMatrixMode(GL_MODELVIEW); }

void setup(GLFWwindow *win) {
  float ratio;
  int width, height;
  glfwGetFramebufferSize(win, &width, &height);
  ratio = width / (float) height;
  glViewport(0, 0, width, height);
  glClear(GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, 1, 0, 1.f, 1.f, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity(); }

void paint(GLFWwindow *win, cam c, int16_t *map) { 
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); glLoadIdentity();
  glTranslatef(-c.x*0.01,-c.y*0.01,0);
  glBegin(GL_POINTS); glColor3f(1,1,1); draw_map(map,SZ2); glEnd(); }

int pressed(GLFWwindow *win,int x) { return glfwGetKey(win,x)!=GLFW_RELEASE; }

cam getInput(GLFWwindow *win) { 
  int l = -pressed(win,GLFW_KEY_LEFT); int r = pressed(win,GLFW_KEY_RIGHT);
  int u = pressed(win,GLFW_KEY_UP); int d = -pressed(win,GLFW_KEY_DOWN);
  int i = pressed(win,GLFW_KEY_I); int o = -pressed(win,GLFW_KEY_O);
  return (cam) { l+r,u+d,i+o }; }

cam parse_input(GLFWwindow *win, cam c) {
  cam e = getInput(win); c = (cam){ c.x+e.x, c.y+e.y, c.z+e.z };
  return c; }

int main(void) { cam c = { 0, 0, 0 };
  int16_t *map = malloc(SZ2*sizeof(int16_t));
  FILE *f = fopen("output.nmp","rb"); in_map(f,&map,SZ2); fclose(f);
  GLFWwindow *window;
  glfwSetErrorCallback(error_callback);
  if (!glfwInit()) exit(EXIT_FAILURE);
  window = glfwCreateWindow(800, 800, "Lietuva", NULL, NULL);
  if (!window) {
      glfwTerminate();
      exit(EXIT_FAILURE); }
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);
  glfwSetKeyCallback(window, key_callback); setup(window);
  glfwSetFramebufferSizeCallback(window, rsz);
  while (!glfwWindowShouldClose(window)) { paint(window,c,map);
    c = parse_input(window,c); glfwSwapBuffers(window); glfwPollEvents(); }
  glfwDestroyWindow(window);
  glfwTerminate();
  exit(EXIT_SUCCESS); free(map); }
