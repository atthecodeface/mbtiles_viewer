#version 410 core
 
in vec4 t_color[];
in vec4 t_normal[];
out vec4 f_color;
out vec3 f_normal;

layout(triangles, equal_spacing, ccw) in;

uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
uniform mat4 G; // GUI matrix - map camera view XYZ1 to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'
 
void main()
{ 
    f_color = ( (t_color[0] * gl_TessCoord.x) +
                (t_color[1] * gl_TessCoord.y) +
                (t_color[2] * gl_TessCoord.z) 
        );
    f_normal = ( (t_normal[0] * gl_TessCoord.x) +
                 (t_normal[1] * gl_TessCoord.y) +
                 (t_normal[2] * gl_TessCoord.z) 
        ).xyz;
    vec4 p4 = ( (gl_TessCoord.x * gl_in[0].gl_Position) +
                (gl_TessCoord.y * gl_in[1].gl_Position) +
                (gl_TessCoord.z * gl_in[2].gl_Position)
        );

    vec3 p3 = vec3(p4.x, p4.y, p4.z);
    float l = length(p3) / length(gl_in[0].gl_Position.xyz);
    vec3 n3 = p3 * sqrt(l);

    vec4 v_s = P*G*vec4(n3,1.0);
    gl_Position = vec4(v_s.xyz, 1.5+v_s.z/10);
}


