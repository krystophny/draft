#ifndef MODULE_H
#define MODULE_H

typedef struct Shape {
    void (*area)(void);
} Shape;

typedef struct Circle {
    Shape shape;
    int radius;
} Circle;

typedef struct Rectangle {
    Shape shape;
    int width;
    int height;
} Rectangle;

void circle_area(void);
void rectangle_area(void);

void circle_init(Circle *circle, int radius);
void rectangle_init(Rectangle *rectangle, int width, int height);

#endif // MODULE_H