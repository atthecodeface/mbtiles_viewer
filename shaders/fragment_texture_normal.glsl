#version 330 core

out vec4 color;

in vec4 f_color;
in vec3 f_normal;
uniform sampler2D T0;
uniform vec3 L;
void main() {
  vec2 f_uv = vec2(f_color.r, f_color.g);
  float brightness = (1.0 + dot ( L, normalize(f_normal) ))/3.0;
  color = brightness * (vec4(0.5)+(texture(T0, f_uv).rgba*0.5));
  color.a = 1;
 }