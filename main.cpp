#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <vector>
#include <cmath>
#include <cstdlib> 

// --- CUSTOMIZATION ---
const int ROLL_NO = 48;
float globalSpeed = 0.5f + ((ROLL_NO % 10) * 0.1f);
// DOUBLED count for a much denser belt
const int ASTEROID_COUNT = 4000;

const int SCREEN_WIDTH = 1200;
const int SCREEN_HEIGHT = 800;
const float PI = 3.14159265359f;

// --- CAMERA ---
glm::vec3 cameraPos = glm::vec3(70.0f, 50.0f, 70.0f); // Moved camera back a bit
glm::vec3 cameraFront = glm::vec3(-0.7f, -0.5f, -0.7f);
glm::vec3 cameraUp = glm::vec3(0.0f, 1.0f, 0.0f);
float deltaTime = 0.0f; float lastFrame = 0.0f; float yaw = -135.0f; float pitch = -30.0f;

// --- SHADERS ---
const char* vertexShaderSource = R"(
    #version 330 core
    layout (location = 0) in vec3 aPos;
    layout (location = 1) in vec3 aNormal;
    layout (location = 2) in vec3 aColor; 

    out vec3 FragPos;
    out vec3 Normal;
    out vec3 VertexColor; 

    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;
    uniform int planetType;

    void main() {
        FragPos = vec3(model * vec4(aPos, 1.0));
        VertexColor = aColor;
        
        // Calculate normal matrix only for objects that need lighting or soft edges
        if(planetType >= 1 && planetType <= 5 || planetType == 7) {
             Normal = normalize(mat3(transpose(inverse(model))) * aNormal);
        } else {
             Normal = vec3(0.0, 1.0, 0.0); 
        }

        gl_Position = projection * view * vec4(FragPos, 1.0);
    }
)";

const char* fragmentShaderSource = R"(
    #version 330 core
    out vec4 FragColor;
    in vec3 Normal;
    in vec3 FragPos;
    in vec3 VertexColor; 

    uniform vec3 objectColor;
    uniform vec3 lightPos; 
    uniform vec3 viewPos;
    uniform float time; 
    // 0=Stars, 1=Matte, 2=Gas, 3=Earth, 4=Asteroid, 5=SunGlow, 6=Tail, 7=Ring, 8=NebulaParticle, 9=MilkyWay
    uniform int planetType; 

    void main() {
        // --- TRANSPARENT / GLOWING OBJECTS ---
        if(planetType == 9) { FragColor = vec4(VertexColor, 0.8); return; } // Milky Way
        
        // NEW: Nebula Particle Shader (Creates a soft, glowing puff)
        if(planetType == 8) { 
            // Calculate distance from the center of the point sprite (approximate)
            vec2 circCoord = 2.0 * gl_PointCoord - 1.0;
            float dist = dot(circCoord, circCoord);
            // Discard pixels outside the circle to make it round
            if (dist > 1.0) discard;
            // Soft fade from center to edge
            float alpha = smoothstep(1.0, 0.0, dist) * 0.15; // Low opacity for layering
            FragColor = vec4(VertexColor, alpha); 
            return; 
        }

        if(planetType == 6) { FragColor = vec4(objectColor, 0.5); return; } // Comet Tail
        
        if(planetType == 5) { // Sun Glow
             vec3 viewDir = normalize(viewPos - FragPos);
             float edge = 1.0 - dot(Normal, viewDir);
             float pulse = (sin(time * 2.0) + 1.0) * 0.5;
             float alpha = 0.2 + (0.1 * pulse);
             FragColor = vec4(objectColor, alpha * edge); 
             return; 
        }
        if(planetType == 0) { FragColor = vec4(objectColor, 1.0); return; } // Stars/Sun Core

        // --- SOLID OBJECTS ---
        vec3 finalColor = objectColor;
        
        // --- UPDATED GAS GIANT LOGIC ---
        if(planetType == 2) { 
            // We add FragPos.x to FragPos.y to create diagonal lines
            // Multiplying by different numbers changes the angle of the tilt
            float band = sin((FragPos.y * 5.0) + (FragPos.x * 4.0)); 
            finalColor = (band > 0.0) ? objectColor*1.1 : objectColor*0.9; 
        }

        if(planetType == 4) { // Asteroid Rock
             float noise = fract(sin(dot(FragPos.xy ,vec2(12.9898,78.233))) * 43758.5453);
             finalColor = objectColor * (0.5 + noise * 0.5); // Slightly brighter base
        }
        if(planetType == 1) {
             float noise = fract(sin(dot(FragPos.xy ,vec2(12.9898,78.233))) * 43758.5453);
             finalColor = objectColor * (0.7 + noise * 0.3);
        }

        // --- LIGHTING ---
        float ambientStrength = 0.02; 
        vec3 ambient = ambientStrength * vec3(1.0);
        vec3 norm = normalize(Normal);
        vec3 lightDir = normalize(lightPos - FragPos);
        float diff = max(dot(norm, lightDir), 0.0);
        vec3 diffuse = diff * vec3(1.3); 

        float specularStrength = 0.5;
        if (planetType == 1 || planetType == 4 || planetType == 7) specularStrength = 0.1; 
        if (planetType == 3) specularStrength = 0.8; 
        
        vec3 viewDir = normalize(viewPos - FragPos);
        vec3 reflectDir = reflect(-lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
        vec3 specular = specularStrength * spec * vec3(1.0);

        float rim = 1.0 - max(dot(viewDir, norm), 0.0);
        rim = pow(rim, 3.0);
        vec3 rimColor = vec3(0.0);
        if(planetType == 2 || planetType == 3) rimColor = vec3(0.1, 0.2, 0.3) * rim;

        vec3 result = (ambient + diffuse + specular) * finalColor + rimColor;
        FragColor = vec4(result, 1.0);
    }
)";

// --- HELPER FUNCTIONS ---
float randomFloat() { return (float)rand() / (float)RAND_MAX; }

float pseudoNoise3D(float x, float y, float z) {
    float ptr = sin(x * 13.0f + y * 17.0f + z * 19.0f);
    float val = sin(ptr) * 43758.5453f;
    return val - floor(val);
}

// --- GEOMETRY GENERATORS ---
struct Sphere {
    std::vector<float> data; std::vector<unsigned int> indices;
    void generate(float radius, int sectors, int stacks) {
        data.clear(); indices.clear();
        float x, y, z, xy, sStep = 2 * PI / sectors, stStep = PI / stacks, sAng, stAng;
        for (int i = 0; i <= stacks; ++i) {
            stAng = PI / 2 - i * stStep; xy = radius * cosf(stAng); z = radius * sinf(stAng);
            for (int j = 0; j <= sectors; ++j) {
                sAng = j * sStep; x = xy * cosf(sAng); y = xy * sinf(sAng);
                data.push_back(x); data.push_back(y); data.push_back(z);
                data.push_back(x / radius); data.push_back(y / radius); data.push_back(z / radius);
                data.push_back(0.0f); data.push_back(0.0f); data.push_back(0.0f);
            }
        }
        int k1, k2;
        for (int i = 0; i < stacks; ++i) {
            k1 = i * (sectors + 1); k2 = k1 + sectors + 1;
            for (int j = 0; j < sectors; ++j, ++k1, ++k2) {
                if (i != 0) { indices.push_back(k1); indices.push_back(k2); indices.push_back(k1 + 1); }
                if (i != (stacks - 1)) { indices.push_back(k1 + 1); indices.push_back(k2); indices.push_back(k2 + 1); }
            }
        }
    }
};

struct IrregularRock {
    std::vector<float> data; std::vector<unsigned int> indices;
    void generate(float radius, int sectors, int stacks) {
        data.clear(); indices.clear();
        float sStep = 2 * PI / sectors; float stStep = PI / stacks;
        for (int i = 0; i <= stacks; ++i) {
            float stAng = PI / 2 - i * stStep;
            float xy = cosf(stAng); float zBase = sinf(stAng);
            for (int j = 0; j <= sectors; ++j) {
                float sAng = j * sStep;
                float xBase = xy * cosf(sAng); float yBase = xy * sinf(sAng);
                float noise = pseudoNoise3D(xBase, yBase, zBase) * 0.35f;
                float finalRadius = radius * (0.8f + noise);
                data.push_back(xBase * finalRadius); data.push_back(yBase * finalRadius); data.push_back(zBase * finalRadius);
                data.push_back(xBase); data.push_back(yBase); data.push_back(zBase);
                data.push_back(0.0f); data.push_back(0.0f); data.push_back(0.0f);
            }
        }
        int k1, k2;
        for (int i = 0; i < stacks; ++i) {
            k1 = i * (sectors + 1); k2 = k1 + sectors + 1;
            for (int j = 0; j < sectors; ++j, ++k1, ++k2) {
                if (i != 0) { indices.push_back(k1); indices.push_back(k2); indices.push_back(k1 + 1); }
                if (i != (stacks - 1)) { indices.push_back(k1 + 1); indices.push_back(k2); indices.push_back(k2 + 1); }
            }
        }
    }
};

struct OrbitPath {
    std::vector<float> vertices;
    void generate() {
        for (int i = 0; i <= 100; i++) {
            float t = 2.0f * PI * float(i) / 100.0f;
            vertices.push_back(cosf(t)); vertices.push_back(0.0f); vertices.push_back(sinf(t));
            vertices.push_back(0); vertices.push_back(1); vertices.push_back(0);
            vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        }
    }
};

struct Stars {
    std::vector<float> vertices;
    void generate(int c) {
        for (int i = 0; i < c; i++) {
            vertices.push_back((rand() % 800) - 400.0f); vertices.push_back((rand() % 800) - 400.0f); vertices.push_back((rand() % 800) - 400.0f);
            vertices.push_back(0); vertices.push_back(1); vertices.push_back(0);
            vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        }
    }
};

struct MilkyWay {
    std::vector<float> data;
    void generate(int count) {
        for (int i = 0; i < count; i++) {
            float theta = randomFloat() * 2.0f * PI;
            float heightRatio = randomFloat(); heightRatio = 1.0f - heightRatio * heightRatio;
            float height = (heightRatio * 70.0f) - 35.0f;
            float distance = 300.0f + randomFloat() * 300.0f;
            float x = distance * cos(theta); float y = height; float z = distance * sin(theta);
            float tilt = glm::radians(50.0f);
            float newY = y * cos(tilt) - z * sin(tilt);
            float newZ = y * sin(tilt) + z * cos(tilt);
            data.push_back(x); data.push_back(newY); data.push_back(newZ);
            data.push_back(0); data.push_back(1); data.push_back(0);
            float distFromCenter = abs(height) / 35.0f;
            glm::vec3 coreColor = glm::vec3(1.0f, 0.95f, 0.8f);
            glm::vec3 edgeColor = glm::vec3(0.3f, 0.2f, 0.6f);
            glm::vec3 starColor = glm::mix(coreColor, edgeColor, sqrt(distFromCenter));
            starColor += glm::vec3(randomFloat() * 0.1f);
            data.push_back(starColor.r); data.push_back(starColor.g); data.push_back(starColor.b);
        }
    }
};

// UPDATED: Nebulas are now made of thousands of small, glowing particles
struct Nebula {
    std::vector<float> data; // Pos(3), Norm(3), Color(3)
    void generate(int clusters, int particlesPerCluster) {
        for (int c = 0; c < clusters; c++) {
            // Random center far away
            glm::vec3 center = glm::vec3((rand() % 1200) - 600.0f, (rand() % 600) - 300.0f, (rand() % 1200) - 600.0f);
            // Random cosmic base color for this cluster
            glm::vec3 baseColor = glm::vec3(0.4f + randomFloat() * 0.4f, 0.1f + randomFloat() * 0.3f, 0.5f + randomFloat() * 0.5f);

            for (int i = 0; i < particlesPerCluster; i++) {
                // Spread particles in a cloud around the center
                glm::vec3 offset = glm::vec3((rand() % 150) - 75.0f, (rand() % 100) - 50.0f, (rand() % 150) - 75.0f);
                glm::vec3 pos = center + offset;

                data.push_back(pos.x); data.push_back(pos.y); data.push_back(pos.z);
                data.push_back(0); data.push_back(1); data.push_back(0); // Dummy normal

                // Vary color slightly per particle
                glm::vec3 pColor = baseColor + glm::vec3(randomFloat() * 0.1f);
                data.push_back(pColor.r); data.push_back(pColor.g); data.push_back(pColor.b);
            }
        }
    }
};

struct ConstellationLines {
    std::vector<float> vertices;
    void addLine(glm::vec3 a, glm::vec3 b) {
        vertices.push_back(a.x); vertices.push_back(a.y); vertices.push_back(a.z);
        vertices.push_back(0); vertices.push_back(1); vertices.push_back(0);
        vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        vertices.push_back(b.x); vertices.push_back(b.y); vertices.push_back(b.z);
        vertices.push_back(0); vertices.push_back(1); vertices.push_back(0);
        vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
    }
    void generate() {
        float s = 12.0f; glm::vec3 offset(-250.0f, 180.0f, -300.0f);
        glm::vec3 p1(0, 0, 0), p2(3, 1, 0), p3(7, 0, 0), p4(10, 2, 0), p5(14, 5, 0), p6(17, 4, 0), p7(16, 7, 0);
        p1 = p1 * s + offset; p2 = p2 * s + offset; p3 = p3 * s + offset; p4 = p4 * s + offset; p5 = p5 * s + offset; p6 = p6 * s + offset; p7 = p7 * s + offset;
        addLine(p1, p2); addLine(p2, p3); addLine(p3, p4); addLine(p4, p5); addLine(p5, p6); addLine(p6, p7); addLine(p7, p4);
        s = 14.0f; offset = glm::vec3(300.0f, -80.0f, -350.0f);
        glm::vec3 o1(0, 5, 0), o2(2, 6, 0), o3(4, 5, 0), o4(-2, 10, 0), o5(6, 10, 0), o6(-1, 0, 0), o7(5, 0, 0);
        o1 = o1 * s + offset; o2 = o2 * s + offset; o3 = o3 * s + offset; o4 = o4 * s + offset; o5 = o5 * s + offset; o6 = o6 * s + offset; o7 = o7 * s + offset;
        addLine(o1, o2); addLine(o2, o3); addLine(o4, o2); addLine(o5, o2); addLine(o2, o6); addLine(o2, o7);
        s = 14.0f; offset = glm::vec3(50.0f, 300.0f, -400.0f);
        glm::vec3 c1(0, 4, 0), c2(2, 0, 0), c3(4, 3, 0), c4(6, 1, 0), c5(8, 5, 0);
        c1 = c1 * s + offset; c2 = c2 * s + offset; c3 = c3 * s + offset; c4 = c4 * s + offset; c5 = c5 * s + offset;
        addLine(c1, c2); addLine(c2, c3); addLine(c3, c4); addLine(c4, c5);
    }
};

struct CrossedTail {
    std::vector<float> vertices;
    void generate() {
        // Plane 1
        vertices.push_back(0); vertices.push_back(-0.5); vertices.push_back(0);   vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        vertices.push_back(0); vertices.push_back(0.5); vertices.push_back(0);   vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        vertices.push_back(0); vertices.push_back(0.0); vertices.push_back(10);  vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        // Plane 2
        vertices.push_back(-0.5); vertices.push_back(0); vertices.push_back(0);   vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        vertices.push_back(0.5); vertices.push_back(0); vertices.push_back(0);    vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
        vertices.push_back(0.0); vertices.push_back(0); vertices.push_back(10);   vertices.push_back(0); vertices.push_back(1); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0); vertices.push_back(0);
    }
};

struct AsteroidData {
    glm::mat4 initialTransform;
    glm::vec3 rotationAxis;
    float rotationSpeed;
};

struct AsteroidBelt {
    std::vector<AsteroidData> asteroids;
    void generate(int count) {
        srand(ROLL_NO);
        for (int i = 0; i < count; i++) {
            AsteroidData ad;
            float angle = (float)i / (float)count * 360.0f;
            float radius = 23.0f + randomFloat() * 6.0f;
            float offset = 7.0f;
            float dispY = (randomFloat() * 2.0f - 1.0f) * offset;
            float dispZ = (randomFloat() * 2.0f - 1.0f) * offset;
            float x = sin(glm::radians(angle)) * radius;
            float y = dispY * 0.2f;
            float z = cos(glm::radians(angle)) * radius;
            float beltTilt = 0.1f;
            float tiltedY = (0.0f + dispY) * cos(beltTilt) - z * sin(beltTilt);
            float tiltedZ = (0.0f + dispY) * sin(beltTilt) + z * cos(beltTilt) + dispZ;
            glm::mat4 model = glm::mat4(1.0f);
            model = glm::translate(model, glm::vec3(x, tiltedY, tiltedZ));
            float scale = 0.1f + randomFloat() * 0.18f; // Slightly larger asteroids
            model = glm::scale(model, glm::vec3(scale));
            ad.initialTransform = model;
            ad.rotationAxis = glm::normalize(glm::vec3(randomFloat(), randomFloat(), randomFloat()));
            ad.rotationSpeed = randomFloat() * 40.0f + 10.0f;
            asteroids.push_back(ad);
        }
    }
};

void processInput(GLFWwindow* window) {
    if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)glfwSetWindowShouldClose(window, true);
    float s = 20.0f * deltaTime;
    if (glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS)cameraPos += s * cameraFront;
    if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)cameraPos -= s * cameraFront;
    if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS)cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * s;
    if (glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS)cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * s;
    float sn = 80.0f * deltaTime;
    if (glfwGetKey(window, GLFW_KEY_RIGHT) == GLFW_PRESS)yaw += sn;
    if (glfwGetKey(window, GLFW_KEY_LEFT) == GLFW_PRESS)yaw -= sn;
    if (glfwGetKey(window, GLFW_KEY_UP) == GLFW_PRESS)pitch += sn;
    if (glfwGetKey(window, GLFW_KEY_DOWN) == GLFW_PRESS)pitch -= sn;
    if (pitch > 89.0f)pitch = 89.0f; if (pitch < -89.0f)pitch = -89.0f;
    glm::vec3 f;
    f.x = cos(glm::radians(yaw)) * cos(glm::radians(pitch));
    f.y = sin(glm::radians(pitch));
    f.z = sin(glm::radians(yaw)) * cos(glm::radians(pitch));
    cameraFront = glm::normalize(f);
}

unsigned int compileShader(unsigned int type, const char* source) {
    unsigned int id = glCreateShader(type); glShaderSource(id, 1, &source, NULL); glCompileShader(id); return id;
}

unsigned int sphereIndicesCount, rockIndicesCount;
unsigned int VAO_Sphere, VAO_Rock, VAO_Orbit, VAO_Stars, VAO_RedStars, VAO_BlueStars, VAO_Galaxy, VAO_Nebula, VAO_Constellations, VAO_Tail;
int modelLoc, colorLoc, planetTypeLoc, viewPosLoc, lightPosLoc, viewLoc, projLoc, timeLoc;

// Helper function to set up VBO attributes for the new 3+3+3 layout
void setupAttribs() {
    // Pos
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    // Normal
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);
    // Color
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)(6 * sizeof(float)));
    glEnableVertexAttribArray(2);
}

void DrawPlanet(float orbitRadius, float orbitSpeed, float size, glm::vec3 color, int type, float time, float inclination, float axialTilt, bool hasRing = false) {
    glUniform1i(planetTypeLoc, 0);
    glm::mat4 modelOrbit = glm::mat4(1.0f);
    modelOrbit = glm::rotate(modelOrbit, glm::radians(inclination), glm::vec3(0.0f, 0.0f, 1.0f));
    modelOrbit = glm::scale(modelOrbit, glm::vec3(orbitRadius));
    glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelOrbit));
    glUniform3f(colorLoc, 0.15f, 0.2f, 0.25f);
    glBindVertexArray(VAO_Orbit); glDrawArrays(GL_LINE_LOOP, 0, 101);

    glUniform1i(planetTypeLoc, type);
    glBindVertexArray(VAO_Sphere);
    glm::mat4 model = glm::mat4(1.0f);
    model = glm::rotate(model, glm::radians(inclination), glm::vec3(0.0f, 0.0f, 1.0f));
    model = glm::rotate(model, time * orbitSpeed * globalSpeed, glm::vec3(0.0f, 1.0f, 0.0f));
    model = glm::translate(model, glm::vec3(orbitRadius, 0.0f, 0.0f));
    model = glm::rotate(model, glm::radians(axialTilt), glm::vec3(0.0f, 0.0f, 1.0f));
    model = glm::rotate(model, time, glm::vec3(0.0f, 1.0f, 0.0f));
    glm::mat4 ringMatrix = model;
    model = glm::scale(model, glm::vec3(size));
    glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
    glUniform3f(colorLoc, color.x, color.y, color.z);
    glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
    if (hasRing) {
        glUniform1i(planetTypeLoc, 7);
        ringMatrix = glm::scale(ringMatrix, glm::vec3(size * 2.2f, 0.05f, size * 2.2f));
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(ringMatrix));
        glUniform3f(colorLoc, 0.85f, 0.8f, 0.65f);
        glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
    }
}

void DrawComet(float time) {
    float speed = time * 0.7f;
    float a = 45.0f; float b = 18.0f;
    float x = a * cos(speed); float z = b * sin(speed); float y = sin(speed) * 12.0f;
    glm::vec3 cometPos(x, y, z);
    glUniform1i(planetTypeLoc, 1); glBindVertexArray(VAO_Sphere);
    glm::mat4 model = glm::mat4(1.0f);
    model = glm::translate(model, cometPos); model = glm::scale(model, glm::vec3(0.5f));
    glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
    glUniform3f(colorLoc, 0.8f, 0.9f, 1.0f);
    glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
    glm::vec3 sunToComet = glm::normalize(cometPos);
    glUniform1i(planetTypeLoc, 6); glBindVertexArray(VAO_Tail);
    glm::mat4 modelTail = glm::mat4(1.0f);
    modelTail = glm::translate(modelTail, cometPos);
    float angleY = atan2(sunToComet.x, sunToComet.z);
    modelTail = glm::rotate(modelTail, angleY, glm::vec3(0.0f, 1.0f, 0.0f));
    float angleX = -asin(sunToComet.y);
    modelTail = glm::rotate(modelTail, angleX, glm::vec3(1.0f, 0.0f, 0.0f));
    modelTail = glm::scale(modelTail, glm::vec3(1.5f, 1.5f, 1.5f));
    glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelTail));
    glUniform3f(colorLoc, 0.7f, 0.8f, 1.0f);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

int main() {
    glfwInit();
    GLFWwindow* window = glfwCreateWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "Solar System Ultimate Realism", NULL, NULL);
    glfwMakeContextCurrent(window);
    glewInit();
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LINE_SMOOTH);
    // Enable point sprites for round particles
    glEnable(GL_PROGRAM_POINT_SIZE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    unsigned int shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, compileShader(GL_VERTEX_SHADER, vertexShaderSource));
    glAttachShader(shaderProgram, compileShader(GL_FRAGMENT_SHADER, fragmentShaderSource));
    glLinkProgram(shaderProgram);
    glUseProgram(shaderProgram);

    // --- GEOMETRY GENERATION ---
    Sphere sphere; sphere.generate(1.0f, 40, 20); sphereIndicesCount = sphere.indices.size();
    IrregularRock rock; rock.generate(0.7f, 16, 10); rockIndicesCount = rock.indices.size();

    unsigned int VBO, EBO;
    glGenVertexArrays(1, &VAO_Sphere); glGenBuffers(1, &VBO); glGenBuffers(1, &EBO);
    glBindVertexArray(VAO_Sphere); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, sphere.data.size() * sizeof(float), sphere.data.data(), GL_STATIC_DRAW); glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO); glBufferData(GL_ELEMENT_ARRAY_BUFFER, sphere.indices.size() * sizeof(unsigned int), sphere.indices.data(), GL_STATIC_DRAW); setupAttribs();

    glGenVertexArrays(1, &VAO_Rock); glGenBuffers(1, &VBO); glGenBuffers(1, &EBO);
    glBindVertexArray(VAO_Rock); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, rock.data.size() * sizeof(float), rock.data.data(), GL_STATIC_DRAW); glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO); glBufferData(GL_ELEMENT_ARRAY_BUFFER, rock.indices.size() * sizeof(unsigned int), rock.indices.data(), GL_STATIC_DRAW); setupAttribs();

    OrbitPath orbit; orbit.generate(); glGenVertexArrays(1, &VAO_Orbit); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Orbit); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, orbit.vertices.size() * sizeof(float), orbit.vertices.data(), GL_STATIC_DRAW); setupAttribs();

    Stars stars; stars.generate(3000); glGenVertexArrays(1, &VAO_Stars); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Stars); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, stars.vertices.size() * sizeof(float), stars.vertices.data(), GL_STATIC_DRAW); setupAttribs();
    Stars redStars; redStars.generate(200); glGenVertexArrays(1, &VAO_RedStars); glGenBuffers(1, &VBO); glBindVertexArray(VAO_RedStars); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, redStars.vertices.size() * sizeof(float), redStars.vertices.data(), GL_STATIC_DRAW); setupAttribs();
    Stars blueStars; blueStars.generate(200); glGenVertexArrays(1, &VAO_BlueStars); glGenBuffers(1, &VBO); glBindVertexArray(VAO_BlueStars); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, blueStars.vertices.size() * sizeof(float), blueStars.vertices.data(), GL_STATIC_DRAW); setupAttribs();

    MilkyWay galaxy; galaxy.generate(30000);
    glGenVertexArrays(1, &VAO_Galaxy); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Galaxy); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, galaxy.data.size() * sizeof(float), galaxy.data.data(), GL_STATIC_DRAW); setupAttribs();

    // UPDATED: Generate particle nebulas
    Nebula nebula; nebula.generate(6, 500); // 6 clusters, 500 particles each
    glGenVertexArrays(1, &VAO_Nebula); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Nebula); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, nebula.data.size() * sizeof(float), nebula.data.data(), GL_STATIC_DRAW); setupAttribs();

    ConstellationLines constellations; constellations.generate(); glGenVertexArrays(1, &VAO_Constellations); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Constellations); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, constellations.vertices.size() * sizeof(float), constellations.vertices.data(), GL_STATIC_DRAW); setupAttribs();
    CrossedTail tail; tail.generate(); glGenVertexArrays(1, &VAO_Tail); glGenBuffers(1, &VBO); glBindVertexArray(VAO_Tail); glBindBuffer(GL_ARRAY_BUFFER, VBO); glBufferData(GL_ARRAY_BUFFER, tail.vertices.size() * sizeof(float), tail.vertices.data(), GL_STATIC_DRAW); setupAttribs();

    AsteroidBelt belt; belt.generate(ASTEROID_COUNT);

    modelLoc = glGetUniformLocation(shaderProgram, "model"); viewLoc = glGetUniformLocation(shaderProgram, "view"); projLoc = glGetUniformLocation(shaderProgram, "projection"); colorLoc = glGetUniformLocation(shaderProgram, "objectColor"); lightPosLoc = glGetUniformLocation(shaderProgram, "lightPos"); planetTypeLoc = glGetUniformLocation(shaderProgram, "planetType"); viewPosLoc = glGetUniformLocation(shaderProgram, "viewPos"); timeLoc = glGetUniformLocation(shaderProgram, "time");

    while (!glfwWindowShouldClose(window)) {
        float time = (float)glfwGetTime(); deltaTime = time - lastFrame; lastFrame = time; processInput(window);
        glClearColor(0.002f, 0.002f, 0.005f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glUniform1f(timeLoc, time);
        glm::mat4 proj = glm::perspective(glm::radians(45.0f), (float)SCREEN_WIDTH / SCREEN_HEIGHT, 0.1f, 1500.0f);
        glm::mat4 view = glm::lookAt(cameraPos, cameraPos + cameraFront, cameraUp);
        glUniformMatrix4fv(projLoc, 1, GL_FALSE, glm::value_ptr(proj));
        glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
        glUniform3f(lightPosLoc, 0.0f, 0.0f, 0.0f);
        glUniform3f(viewPosLoc, cameraPos.x, cameraPos.y, cameraPos.z);

        // 1. BACKGROUND LAYERS (Order matters for transparency)
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(glm::mat4(1.0f)));

        // Milky Way
        glUniform1i(planetTypeLoc, 9);
        glPointSize(1.5f); glBindVertexArray(VAO_Galaxy); glDrawArrays(GL_POINTS, 0, galaxy.data.size() / 9);

        // UPDATED: Nebula Particles (Draw with disabled depth write for blending)
        glDepthMask(GL_FALSE);
        glUniform1i(planetTypeLoc, 8);
        glPointSize(60.0f); // Large, soft particles
        glBindVertexArray(VAO_Nebula); glDrawArrays(GL_POINTS, 0, nebula.data.size() / 9);
        glDepthMask(GL_TRUE);

        // Stars
        glUniform1i(planetTypeLoc, 0);
        glPointSize(1.2f); glUniform3f(colorLoc, 0.8f, 0.8f, 0.85f); glBindVertexArray(VAO_Stars); glDrawArrays(GL_POINTS, 0, stars.vertices.size() / 9);
        glPointSize(2.8f); glUniform3f(colorLoc, 1.0f, 0.5f, 0.4f); glBindVertexArray(VAO_RedStars); glDrawArrays(GL_POINTS, 0, redStars.vertices.size() / 9);
        glPointSize(2.8f); glUniform3f(colorLoc, 0.4f, 0.7f, 1.0f); glBindVertexArray(VAO_BlueStars); glDrawArrays(GL_POINTS, 0, blueStars.vertices.size() / 9);
        glPointSize(1.0f);

        // Constellations
        glLineWidth(1.0f); glUniform3f(colorLoc, 0.6f, 0.8f, 1.0f); glBindVertexArray(VAO_Constellations); glDrawArrays(GL_LINES, 0, constellations.vertices.size() / 9);

        // 4. SUN
        glBindVertexArray(VAO_Sphere);
        glUniform1i(planetTypeLoc, 0); glm::mat4 modelSun = glm::scale(glm::mat4(1.0f), glm::vec3(4.0f)); glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelSun)); glUniform3f(colorLoc, 1.0f, 0.9f, 0.5f); glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
        glDepthMask(GL_FALSE);
        glUniform1i(planetTypeLoc, 5); glm::mat4 modelGlow = glm::scale(glm::mat4(1.0f), glm::vec3(6.5f)); glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelGlow)); glUniform3f(colorLoc, 1.0f, 0.7f, 0.2f); glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
        glDepthMask(GL_TRUE);

        // 5. PLANETS
        DrawPlanet(7.0f, 1.5f, 0.8f, glm::vec3(0.6f, 0.6f, 0.65f), 1, time, 7.0f, 0.0f);
        DrawPlanet(11.0f, 1.2f, 1.1f, glm::vec3(0.85f, 0.75f, 0.6f), 3, time, 3.4f, 177.0f);
        {
            glUniform1i(planetTypeLoc, 0); glm::mat4 modelOrbit = glm::scale(glm::mat4(1.0f), glm::vec3(15.0f)); glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelOrbit)); glUniform3f(colorLoc, 0.15f, 0.2f, 0.25f); glBindVertexArray(VAO_Orbit); glDrawArrays(GL_LINE_LOOP, 0, 101);
            glUniform1i(planetTypeLoc, 3); glBindVertexArray(VAO_Sphere); glm::mat4 modelEarth = glm::rotate(glm::mat4(1.0f), time * 1.0f * globalSpeed, glm::vec3(0.0f, 1.0f, 0.0f)); modelEarth = glm::translate(modelEarth, glm::vec3(15.0f, 0.0f, 0.0f)); modelEarth = glm::rotate(modelEarth, glm::radians(23.5f), glm::vec3(0.0f, 0.0f, 1.0f)); glm::mat4 earthDraw = glm::scale(modelEarth, glm::vec3(1.2f)); glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(earthDraw)); glUniform3f(colorLoc, 0.1f, 0.35f, 0.8f); glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
            glUniform1i(planetTypeLoc, 1); glm::mat4 modelMoon = modelEarth; modelMoon = glm::rotate(modelMoon, time * 3.0f, glm::vec3(0.0f, 1.0f, 0.0f)); modelMoon = glm::translate(modelMoon, glm::vec3(2.0f, 0.0f, 0.0f)); modelMoon = scale(modelMoon, glm::vec3(0.4f)); glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(modelMoon)); glUniform3f(colorLoc, 0.9f, 0.9f, 0.92f); glDrawElements(GL_TRIANGLES, sphereIndicesCount, GL_UNSIGNED_INT, 0);
        }
        DrawPlanet(20.0f, 0.8f, 0.9f, glm::vec3(0.8f, 0.4f, 0.3f), 1, time, 1.8f, 25.0f);
        DrawPlanet(29.0f, 0.5f, 3.5f, glm::vec3(0.75f, 0.65f, 0.55f), 2, time, 1.3f, 3.0f);
        DrawPlanet(39.0f, 0.4f, 3.0f, glm::vec3(0.85f, 0.8f, 0.6f), 2, time, 2.5f, 26.0f, true);
        DrawPlanet(49.0f, 0.3f, 2.0f, glm::vec3(0.5f, 0.8f, 0.8f), 2, time, 0.8f, 98.0f);
        DrawPlanet(58.0f, 0.2f, 1.9f, glm::vec3(0.2f, 0.2f, 0.7f), 2, time, 1.8f, 28.0f);

        // 6. REALISTIC ASTEROIDS (Dense and Distinct)
        glUniform3f(colorLoc, 0.5f, 0.45f, 0.4f); // Slightly brighter rock color
        glUniform1i(planetTypeLoc, 4); // Rock shader
        glBindVertexArray(VAO_Rock); // Use lumpy rock mesh
        for (const auto& ad : belt.asteroids) {
            glm::mat4 model = ad.initialTransform;
            // Apply individual tumbling rotation
            model = glm::rotate(model, glm::radians(time * ad.rotationSpeed), ad.rotationAxis);
            glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
            glDrawElements(GL_TRIANGLES, rockIndicesCount, GL_UNSIGNED_INT, 0);
        }

        // 7. COMET
        glDepthMask(GL_FALSE); DrawComet(time); glDepthMask(GL_TRUE);

        glfwSwapBuffers(window); glfwPollEvents();
    }
    glfwTerminate(); return 0;
}