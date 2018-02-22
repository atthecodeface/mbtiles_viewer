#version 330 core

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec3 color;
uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
out vec4 v_color;
out vec4 v_normal;
  void main()
  {
    mat4 VM;
    vec4 v_w;
    vec4 v_v;
 
    v_w = M*vec4(vertex,1.0);
    v_v = V*v_w;
    v_v.w = 1.0;

    gl_Position = v_v;
    v_normal = M*vec4(normal,0.0);
    v_color = vec4(color,1.0);

  }
