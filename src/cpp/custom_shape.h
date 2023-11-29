#pragma once

#include <set>
#include <vector>
#include <oglwrap/buffer.h>
#include <oglwrap/context.h>
#include <oglwrap/vertex_array.h>
#include <oglwrap/vertex_attrib.h>

struct MeshPoint 
{
    glm::vec3 pos;
    glm::vec3 norm;
    glm::vec2 uv;
    int tex_id;
    MeshPoint () = default;
    MeshPoint(
        float p1, float p2, float p3, 
        float p4, float p5, float p6, 
        float p7, float p8, int p9=0)
        : pos{p1, p2, p3}, norm{p4, p5, p6}, uv{p7, p8}, tex_id{p9}
    {}
};

class Mesh 
{
private:
    std::vector<MeshPoint>* m_points;
    std::vector<unsigned>* m_indices;
    gl::VertexArray m_vao;
    gl::ArrayBuffer m_buffer;
    gl::IndexBuffer m_ind_buffer;

    void UpdateVao ();

public:
    Mesh ();

    /// Render the mesh.
    void Render();

    /// Set positions.
    void Set (std::vector<MeshPoint>* points, std::vector<unsigned>* indices)
    {
        m_points = points;
        m_indices = indices;
        UpdateVao ();
    }
};

/// 2 or 3 D curve
class Curve 
{
private:
    std::vector<glm::vec3> m_positions;
    gl::VertexArray m_vao;
    gl::ArrayBuffer m_buffer;

    void UpdatePositions ();

public:
    Curve (); 

    /// Render the curve.
    void Render();

    /// Extend the curve by adding a new point.
    void AddPoint(glm::vec3 const& point) 
    {
        m_positions.push_back(point);
        UpdatePositions();
    }

    /// Set positions of vertices in curve.
    void SetPositions(std::vector<glm::vec3>&& positions)
    {
        m_positions = std::move(positions);
        UpdatePositions();
    }

    std::vector<glm::vec3> GetPositions () const {return m_positions;} 
};

#include "custom_shape-inl.h"
