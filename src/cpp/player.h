#pragma once

#include "custom_shape.h"

class Player 
{
private:
    glm::vec3 m_force;
    glm::vec3 m_vel;
    glm::vec3 m_pos;

public:
    glm::vec3 m_move_vel;

    Player (glm::vec3 pos = glm::vec3(0.0f)) : m_force{0,0,0}, m_vel{0,0,0}, m_pos{pos} {}

    void Integrate (float dt)
    {
        m_vel += dt * m_force;
        m_pos += dt * (m_vel + m_move_vel);
    }

    void SetForce (glm::vec3 const& force) 
    {
        m_force = force;
    }

    void AddForce (glm::vec3 const& force) 
    {
        m_force += force;
    }

    glm::vec3 const& GetVel () const {return m_vel;}
    void SetVel (glm::vec3 const& vel)
    {
        m_vel = vel;
    }


    void ResetVelocity () {m_vel = {0,0,0};}
    void ResetForce () {m_force = {0,0,0};}

    glm::vec3 const& GetPos () const {return m_pos;}
    void SetPos (glm::vec3 const& pos) {m_pos = pos;}
};
