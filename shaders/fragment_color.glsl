#version 330 core
//in vec3 V_m;
//in vec2 V_UV;
//in vec3 N_m;
out vec4 color;
uniform vec3 C;
void main(){
    color = vec4(C,1);
}
