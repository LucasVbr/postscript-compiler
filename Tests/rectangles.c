/* Declaration of native Postscript procedures / functions */

void newpath ();
void moveto(float x, float y);
void rlineto(float x, float y);
void closepath();
void stroke ();

/* Function definition */

void rectangle(float x, float y, float a, float b) {
  newpath();
  moveto(x, y);
  rlineto(a, 0.);
  rlineto(0., b);
  rlineto(0. -. a, 0.);
  rlineto(0., 0. -. b);
  closepath();
  stroke();
}

void square(float x, float y, float a) {
  rectangle(x, y, a, a);
}

void fig1a(int d, float x, float y, float a) {
  if (d != 0) {
    square(x, y, a);
    fig1a(d - 1, x, y, a /. 2.);
  }
}

void fig1b(int d, float x, float y, float a) {
  if (d != 0) {
    square(x, y, a);
    fig1b(d - 1, x +. a, y +. a, a /. 2.);
  }
}

void main () {
  fig1a(5, 200., 450., 100.);
  fig1b(5, 100., 200., 100.);
}

