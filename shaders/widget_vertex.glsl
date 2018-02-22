#version 330 core

layout(location = 0) in vec3 vertex;
uniform mat4 G; // GUI matrix - map camera view to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'
uniform float spin;
out vec4 v_color;
  void main()
  {
    gl_Position =  P * G * vec4(vertex,1);
  }
