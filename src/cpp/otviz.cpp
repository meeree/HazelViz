#include "oglwrap_example.hpp"
#include <boost/python.hpp>

#include <oglwrap/oglwrap.h>
#include "custom_shape.h"
#include <oglwrap/shapes/cube_shape.h>
#include <glm/gtc/matrix_transform.hpp>
#include <lodepng.h>
#include <utility>
#include <numeric>
#include <boost/python/stl_iterator.hpp>

#include "player.h"

void MeshGen(std::vector<float> const& pts, std::vector<unsigned> const& inds, std::vector<float> const& normals,
         std::vector<MeshPoint>& points, std::vector<unsigned>& inds_out)
{
    auto pt_itr = pts.begin(), nm_itr = normals.begin();
    for(int i = 0; i < pts.size() / 3; ++i)
    {
        points.emplace_back(*pt_itr++, *pt_itr++, *pt_itr++, *nm_itr++, *nm_itr++, *nm_itr++, 0, 0, 9);
    }

    int cur_size = inds_out.size();
    inds_out.resize(cur_size + inds.size());
    std::copy(inds.begin(), inds.end(), inds_out.begin() + cur_size);
}

class Graphics : public OglwrapExample {
    private:
        gl::Program prog_;
        gl::Texture2D tex_;

        glm::vec2 camAng = {0, -M_PI_2};
        glm::vec3 camForward = {1, 0, 0};

        Mesh petMesh;
        std::vector<MeshPoint> petMeshPoints_;
        std::vector<unsigned> petMeshInds_;
        Player player_;

    public:
        Graphics (std::vector<float> const& pts, std::vector<unsigned> const& inds, std::vector<float> const& normals)
            : player_(glm::vec3(0.5,1,0))
        {
            MeshGen(pts, inds, normals, petMeshPoints_, petMeshInds_);
            petMesh.Set(&petMeshPoints_, &petMeshInds_);

            // We need to add a few more lines to the shaders
            gl::ShaderSource vs_source;
            vs_source.set_source(R"""(
      #version 330 core
      layout (location=0) in vec3 inPos;
      layout (location=1) in vec3 inNormal;
      layout (location=2) in vec2 inUV;
      layout (location=3) in int inTexId;

      uniform mat4 mvp;

      out vec3 normal;
      out vec3 position;
      out vec2 uv;
      flat out int tex_id;

      void main() {
        normal = inNormal;
        position = inPos;
        uv = inUV;
        tex_id = inTexId;
        gl_Position = mvp * vec4(inPos, 1.0);
      })""");
            vs_source.set_source_file("example_shader.vert");
            gl::Shader vs(gl::kVertexShader, vs_source);

            gl::ShaderSource fs_source;
            fs_source.set_source(R"""(
      #version 330 core
      in vec3 normal;
      in vec3 position;
      in vec2 uv;
      flat in int tex_id;

      uniform vec3 lightPos1;
      uniform sampler2D tex;
      uniform int sprite_dim;

      out vec4 fragColor;

      void main() {
        vec3 lightVec1 = normalize(lightPos1 - position);
        float d1 = abs(dot(lightVec1, normal)); // Note the abs means normal sign does not matter.
        vec3 c1 = vec3(1);  

        // Retrieve bottom left coordinate of sprite from sprite sheet.
        vec2 sprite_coords = vec2(tex_id % sprite_dim, tex_id / sprite_dim + 1);
        sprite_coords = (sprite_coords + vec2(uv.x, -uv.y)) / float(sprite_dim);
        vec4 tex_col = texture(tex, sprite_coords);
		vec3 ambient = 0.8 * vec3(1.0);
        fragColor = tex_col * vec4(ambient + d1 * c1, 1.0);
        fragColor = vec4(normal, 1.0) + 0.000001 * fragColor;
      })""");
            fs_source.set_source_file("example_shader.frag");
            gl::Shader fs(gl::kFragmentShader, fs_source);
            gl::PointSize(4);

            // Create a shader program
            prog_.attachShader(vs);
            prog_.attachShader(fs);
            prog_.link();
            gl::Use(prog_);

            // Bind the attribute locations
            (prog_ | "inPos").bindLocation(0);
            (prog_ | "inNormal").bindLocation(1);
            (prog_ | "inUV").bindLocation(2);

            gl::Enable(gl::kDepthTest);

            // Set the clear color
            gl::ClearColor(0.503, 0.829, 0.993, 1.0f);

            // Setup texture.
            {
                gl::Bind(tex_);
                unsigned width, height;
                std::vector<unsigned char> data;
                std::string path = "../minecraft_sprites_real.png";
                unsigned error = lodepng::decode(data, width, height, path, LCT_RGBA, 8);
                if (error) {
                    std::cerr << "Image decoder error " << error << ": " << lodepng_error_text(error) << std::endl;
                    std::terminate();
                }
                tex_.upload(gl::kSrgb8Alpha8, width, height,
                        gl::kRgba, gl::kUnsignedByte, data.data());
                tex_.generateMipmap();

                // Want to have some fun? Change kNearest to KLinear. 
                tex_.minFilter(gl::kNearest);
                tex_.magFilter(gl::kNearest);
                tex_.wrapS(gl::kRepeat);
                tex_.wrapT(gl::kRepeat);
            }

            gl::Uniform<int>(prog_, "sprite_dim") = 32;

            // Disable cursor.
            glfwSetInputMode(window_, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
            glfwSetInputMode(window_, GLFW_STICKY_KEYS, false);

            // Set callbacks.
            glfwSetMouseButtonCallback(window_, MouseDiscreteCallback);

            // Set user pointer. This is used in the callbacks, which are static.
            glfwSetWindowUserPointer(window_, this);

            // Enable alpha blending
            gl::Enable(gl::kBlend);
            gl::BlendFunc(gl::kSrcAlpha, gl::kOneMinusSrcAlpha);
        }

    protected:
        virtual void Render() override {
            float t = glfwGetTime();

            // Player position update.
            player_.Integrate(0.01);
            glm::vec3 pos = player_.GetPos();

            glm::mat4 camera_mat = glm::lookAt(player_.GetPos(), player_.GetPos() + camForward, glm::vec3{0.0f, 1.0f, 0.0f});
            glm::mat4 model_mat = glm::mat4x4(1.0f);
            glm::mat4 proj_mat = glm::perspectiveFov<float>(M_PI/2.0, kScreenWidth, kScreenHeight, 0.0001f, 50);
            gl::Uniform<glm::mat4>(prog_, "mvp") = proj_mat * camera_mat * model_mat;

            glm::vec3 lightPos1 = player_.GetPos();
            gl::Uniform<glm::vec3>(prog_, "lightPos1") = lightPos1;

            // Render pet.
            model_mat = glm::mat4x4(1.0f);
            gl::Uniform<glm::mat4>(prog_, "mvp") = proj_mat * camera_mat * model_mat;
            petMesh.Render();

            HandleKeys();
            HandleMouse();
        }

        // Only called when mouse events (release/press) happen. This is good when you 
        // don't want to register multiple clicks when the user clicks once.
        static void MouseDiscreteCallback(GLFWwindow* window, int button, int action, int mods)
        {
//            Graphics* g = static_cast<Graphics*>(glfwGetWindowUserPointer(window));
        }

        void HandleMouse()
        {
            double xpos, ypos;
            glfwGetCursorPos(window_, &xpos, &ypos);
            glfwSetCursorPos(window_, kScreenWidth / 2., kScreenHeight / 2.);

            // Convert xpos, ypos from screen coordinates to [-1,1] coordinates.
            glm::vec2 wpos = {(float)xpos, (float)ypos};
            wpos = glm::vec2(2) * wpos / glm::vec2(kScreenWidth, -kScreenHeight);
            wpos += glm::vec2(-1, 1);

            // Update camera angle. Notice that we restrict the y angle 
            // because we don't want to flip the camera by crossing pi/2 or -pi/2.
            camAng.x += wpos.x * 0.2f;
            float new_cam_y = camAng.y + wpos.y * 0.2f;
            if(fabs(new_cam_y) < M_PI_2) 
                camAng.y = new_cam_y;

            camForward = {cos(camAng.x) * cos(camAng.y), 
                       sin(camAng.y), 
                       sin(camAng.x) * cos(camAng.y)};
        }

        void HandleKeys()
        {
            float moveSpeed = 0.5;

            // Sprinting
            if(glfwGetKey(window_, GLFW_KEY_LEFT_SHIFT) == GLFW_PRESS)
                moveSpeed *= 2;

            // Handle movement with WASD
            bool gW = glfwGetKey(window_, GLFW_KEY_W) == GLFW_PRESS;
            bool gA = glfwGetKey(window_, GLFW_KEY_A) == GLFW_PRESS;
            bool gS = glfwGetKey(window_, GLFW_KEY_S) == GLFW_PRESS;
            bool gD = glfwGetKey(window_, GLFW_KEY_D) == GLFW_PRESS;
            bool gQ = glfwGetKey(window_, GLFW_KEY_Q) == GLFW_PRESS;
            bool gE = glfwGetKey(window_, GLFW_KEY_E) == GLFW_PRESS;
            if(gW | gA | gS | gD | gQ | gE)
            {
                int dir = (gW | gD) ? 1 : -1; // Positive or negative dir?
                float ang_off_ = (gW | gS) ? 0 : M_PI_2; // Forward or to side?
                float updown = (gQ | gE) ? (gQ ? -1 : 1) : 0;
                player_.m_move_vel = dir * moveSpeed * 
                        glm::vec3(cos(camAng.x + ang_off_), updown, sin(camAng.x + ang_off_));
            }
            else 
            {
                player_.m_move_vel = glm::vec3(0.0f);
            }

            // Jumping
            if(glfwGetKey(window_, GLFW_KEY_SPACE) == GLFW_PRESS)
                player_.SetVel({0, 1, 0});

            // End game
            if(glfwGetKey(window_, GLFW_KEY_ESCAPE) == GLFW_PRESS)
            {
                glfwSetWindowShouldClose(window_, 1);
            }
        }

};

int main()
{
}

template< typename T >
inline
std::vector< T > to_std_vector( const boost::python::object& iterable )
{
    return std::vector< T >( boost::python::stl_input_iterator< T >( iterable ),
                             boost::python::stl_input_iterator< T >( ) );
}

void draw_triangles(const boost::python::list& pts, const boost::python::list& inds, const boost::python::list& normals)
{
    auto pts_vec = to_std_vector<float>(pts);
    auto inds_vec = to_std_vector<unsigned int>(inds);
    auto normals_vec = to_std_vector<float>(normals);
    Graphics(pts_vec, inds_vec, normals_vec).RunMainLoop();
}

BOOST_PYTHON_MODULE(otviz)
{
    using namespace boost::python;
    def("draw_triangles", draw_triangles);
}
