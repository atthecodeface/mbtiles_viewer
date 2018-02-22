#version 330 core
out vec4 color;
uniform vec3 C;
void main(){
    color = vec4(C,1);
}
