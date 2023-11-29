#pragma once

#include "./custom_shape.h"
#include <algorithm>

Curve::Curve ()
{
    // Create positions vertex attribute pointer.
    UpdatePositions();
}

void Curve::UpdatePositions () 
{
    // Set indices data.
    Bind(m_vao);
    Bind(m_buffer);
    gl::VertexAttrib(0).pointer(3, gl::DataType::kFloat, false, 0, nullptr).enable();
    m_buffer.data(m_positions);
    Unbind(m_buffer);
    Unbind(m_vao);
}

void Curve::Render () 
{
    Bind(m_vao);
    gl::DrawArrays(gl::PrimType::kLineStrip, 0, m_positions.size());
    Unbind(m_vao);
}

Mesh::Mesh () 
{
    // Create positions vertex attribute pointer.
    m_points = nullptr;
    m_indices = nullptr;
}

void Mesh::UpdateVao () 
{
    // Set indices data.
    Bind(m_vao);
    Bind(m_buffer);
    Bind(m_ind_buffer);
    size_t stride = sizeof(MeshPoint);
    gl::VertexAttrib(0).pointer(3, gl::DataType::kFloat, false, stride, nullptr).enable();
    gl::VertexAttrib(1).pointer(3, gl::DataType::kFloat, false, stride, (void*)(sizeof(float)*3)).enable();
    gl::VertexAttrib(2).pointer(2, gl::DataType::kFloat, false, stride, (void*)(sizeof(float)*6)).enable();
    gl::VertexAttrib(3).pointer(3, gl::DataType::kFloat, false, stride, (void*)(sizeof(float)*8)).enable();
    m_buffer.data(*m_points);
    m_ind_buffer.data(*m_indices);
    Unbind(m_ind_buffer);
    Unbind(m_buffer);
    Unbind(m_vao);
}

void Mesh::Render () 
{
    Bind(m_vao);
    Bind(m_ind_buffer);
    gl::DrawElements(gl::PrimType::kTriangles, m_indices->size(), gl::IndexType::kUnsignedInt);
    Unbind(m_ind_buffer);
    Unbind(m_vao);
}
