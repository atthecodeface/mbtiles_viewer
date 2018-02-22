#version 330 core
layout(location = 0) in vec3 V_m;
//layout(location = 1) in vec2 V_UV;
//layout(location = 2) in vec3 N_m;
//out vec2 UV;
//out vec3 V_w;
//out vec3 V_c;
uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
uniform mat4 G; // GUI matrix - map camera view to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map camera view to projection and where it is on the OpenGL 'page'
uniform float spin;
void main(){
    vec3 V_m2;
//    V_w = (M * vec4(V_m,1)).xyz;// + 0*N_m;// Use N_m or lose it...
//    V_c = (V * M * vec4(V_m,1)).xyz;
    V_m2 = V_m;
    V_m2.x = V_m.x;// + spin*V_m.z;
    gl_Position =  P * G * V * M * vec4(V_m2,1);
    //    UV = V_UV;
}
