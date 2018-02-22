#version 330 core

layout(location = 0) in vec3 vertex;
uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
uniform mat4 G; // GUI matrix - map camera view to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'
out vec4 v_color;
  void main()
  {
    vec4 v_in;
    vec4 v_w;
    vec4 v_v;
    vec4 v_g;
    vec4 v_s;
    v_in = vec4(vertex,1.0);
    v_w = M * v_in;
    v_v = V * vec4(v_w.x,v_w.y,v_w.z,1.0);
    v_v.w = 1.0;
    v_g = G * v_v;
    v_g.z = -v_g.z/20.0;
    v_s = P * v_g;
    v_color     = vec4(1.0,1.0,1.0,1.0);
    gl_Position = vec4(v_s.xyz, 1.2+v_s.z/5.0);
  }