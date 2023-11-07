#include "headers/Solver.hpp"

/***********************************************************************************************************************
 *                                                                                                                     *
 *                                                                                                                     *
 *                                                Search engine methods                                                *
 *                                                                                                                     *
 *                                                                                                                     *
 ***********************************************************************************************************************/

/**
 * Creates a search engine for the given problem
 * @param pb an instance of the Counterpoint class representing a given problem
 * @param type the type of search engine to create (see enumeration in headers/gecode_problem.hpp)
 * @return a search engine for the given problem
 */
Search::Base<Counterpoint>* make_solver(Counterpoint* pb, int type){
    std::cout << "Entering Solver class" << std::endl;

    Gecode::Search::Options opts; //@todo add options when necessary

    std::cout << "Returning the solver" << std::endl;
    if (type == BAB_SOLVER){
        write_to_log_file("Solver type: BAB\n");
        return new BAB<Counterpoint>(pb, opts);
    }
    else{
        write_to_log_file("Solver type: DFS\n");
        return new DFS<Counterpoint>(pb, opts);
    }
}

/**
 * Returns the next solution space for the problem
 * @param solver a solver for the problem
 * @return an instance of the Counterpoint class representing the next solution to the problem
 */
Counterpoint* get_next_solution_space(Search::Base<Counterpoint>* solver){
    std::cout << "Calling next solution on sol_space: " << std::endl;
    std::cout << "Solver = " << solver << std::endl;
    Counterpoint* sol_space = solver->next();
    std::cout << "Got next solution on sol_space: " << sol_space << std::endl;
    if (sol_space == nullptr) // handle the case of no solution or time out, necessary when sending the data to OM
        return nullptr;
    return sol_space;
}

/**
 * Returns the best solution for the problem pb. It uses a branch and bound solver with lexico-minimization of the costs
 * @param pb an instance of a Counterpoint problem
 * @return the best solution to the problem
 */
const Counterpoint* find_best_solution(Counterpoint *pb){
    // create a new search engine
    auto* solver = make_solver(pb, BAB_SOLVER);

    write_to_log_file("Searching for the optimal solution based on preferences:\n");

    Counterpoint *bestSol; // keep a pointer to the best solution
    while(Counterpoint *sol = get_next_solution_space(solver)){
        bestSol = sol;
    }
    write_to_log_file(bestSol->to_string().c_str());
    return bestSol;
}

/**
 * Returns the first maxNOfSols solutions for the problem pb using the solver solverType.
 * @param pb an instance of a Counterpoint problem
 * @param solverType the type of the solver to use from solver_types
 * @param maxNOfSols the maximum number of solutions we want to find (the default value is 1000)
 * @return the first maxNOfSols solutions to the problem
 */
vector<const Counterpoint*> find_all_solutions(Counterpoint *pb, int solverType, int maxNOfSols){
    vector<const Counterpoint*> sols;
    // create the search engine
    auto* solver = make_solver(pb, solverType);
    write_to_log_file("\n Searching for all solutions to the problem with the given solver type:");

    int nbSol = 0;
    while(Counterpoint *sol= get_next_solution_space(solver)){
        nbSol++;
        sols.push_back(sol);
        string message = "Solution" + to_string(nbSol) + ": \n" + sol->to_string() + "\n";
        write_to_log_file(message.c_str());
        if (nbSol >= maxNOfSols)
            break;
    }
    return sols;
}