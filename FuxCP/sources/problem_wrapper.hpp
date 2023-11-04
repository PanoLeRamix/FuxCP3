#ifndef gecode_wrapper_hpp
#define gecode_wrapper_hpp

#include <stdlib.h>

#include "Utilities.hpp"
#include "counterpoint.hpp"
#include "Solver.hpp"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Wraps the FourVoiceTexture constructor.
 * @todo modify this to include any parameters your FourVoiceTexture constructor requires
 * @param size an integer representing the size of the problem
 * @param chord_degrees an integer array representing the chord degrees
 * @param chord_positions an integer array representing the chord positions
 * @return A pointer to a FourVoiceTexture object casted as a void*
 */
void* create_new_problem(int size, int* cf);

/**
 * returns the size of the problem
 * @param sp a void* pointer to a FourVoiceTexture object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp);

/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a FourVoiceTexture object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp);

/**
 * creates a search engine for FourVoiceTexture objects
 * @param sp a void* pointer to a FourVoiceTexture object
 * @return a void* cast of a Base<FourVoiceTexture>* pointer
 */
void* create_solver(void* sp, int type);

/**
 * returns the best solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a BAB<FourVoiceTexture> for the search engine of the problem
 * @return a void* cast of a FourVoiceTexture* pointer
 */
void* return_best_solution_space(void* solver);

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a Base<FourVoiceTexture>* pointer for the search engine of the problem
 * @return a void* cast of a FourVoiceTexture* pointer
 */
void* return_next_solution_space(void* solver);

#ifdef __cplusplus
};
#endif

#endif