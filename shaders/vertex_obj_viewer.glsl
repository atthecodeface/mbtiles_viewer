#version 330 core

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 color;
uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
uniform mat4 G; // GUI matrix - map camera view to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'
out vec4 v_color;
  void main()
  {
    vec4 v_w;
    vec4 v_v;
    vec4 v_g;
    vec4 v_c;
    vec3 c;
    float brightness;
    brightness = 1.0;
    v_w = M*vec4(vertex,1.0);
    v_v = V*vec4(v_w.x,v_w.y,v_w.z,1.0);
    v_v.w = 1.0;
    v_g = G*v_v;
    v_c = P*v_g;
    brightness = 0.5+v_v.z*0.5; // 1.0 when z=1, 0.2 when z=-1,
    brightness = (brightness<0)?0:((brightness>1.0)?1.0:brightness);
    c = color*brightness+vec3(1.0,1.0,1.0)*(1-brightness)*0.5;
    v_color     = vec4(c, 1.0);
    v_c.z = -v_c.z;
    //    gl_Position = vec4(v_c.xyz, 1.5+v_c.z);
    gl_Position = vec4(v_c.xyz, 1.5+v_c.z);
  }