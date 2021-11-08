#include "MelodizerLNS.h"

using namespace Gecode;
using namespace std;

//TODO : change bool major for an int to make it include diminished and augmented
int MAJOR(0);
int MINOR(1);
int DIMINISHED(2);
int AUGMENTED(3);

void printIntVector(vector<int> v){
  for(int i = 0; i < v.size(); ++i){
    std::cout << v[i] << " ";
  }
  std::cout << std::endl;
}

void printIntVectorVector(vector<vector<int>> v){
  for (int i = 0; i < v.size(); i++) {
    for (auto it = v[i].begin(); it != v[i].end(); it++){
      cout << *it << " ";
    }
    cout << endl;
  }
}

class MelodizerLNS : public Space {
  protected:
    //Melody
    IntVarArray pitch;  //the pitch 
    vector<int> duration;    //the duration of a note expressed in sixteenth notes
    vector<int> start;  //the start of the note with respect to the start of the piece expressed in sixteenth notes
    IntVarArray intervals;  //the intervals between each adjacent notes

    //Chords
    const vector<vector<int>> chords;   //the list of input chords
    const vector<int> chordDuration;    //the duration of the chords
    const vector<int> chordStart;   //the starting point of the chords

    int n;  //the number of notes
    int scale[7]; //The scale corresponding to the key and the mode

    //intervals depending on the mode
    const int majorScale[7] = {2, 2, 1, 2, 2, 2, 1};
    const int minorScale[7] = {2, 1, 2, 2, 1, 2, 2};

    const vector<vector<int>> majorChords{{4, 3}, {3, 5}, {5, 4}};
    const vector<vector<int>> minorChords{{3, 4}, {4, 5}, {5, 3}};
    const vector<vector<int>> diminishedChords{{3, 3}, {3, 6}, {6, 3}};

    //sensitive notes
    vector<int> firsts;
    vector<int> seconds;
    vector<int> thirds;
    vector<int> fourths;
    vector<int> fifths;
    vector<int> sixths;
    vector<int> sevenths;

    //admissible intervals with the highest note of the chord
    const vector<int> admissibleIntervalsWithHighNoteFromChord{3, 4, 5, 7, 8, 9, 12};
    
    const IntSet admissibleIntervalsBetweenAdjacentNotes{-1, -2, -3, -4, -5, -7, -8, -9, -12,
                                                          0, 1, 2, 3, 4, 5, 7, 8, 9, 12};

    int minimum;

  public:
    IntVarArray getPitch(){
      return pitch;
    }
    /*
      dissonnance resolution : les sensibles (4ème ou 7ème) vont vers la tonique (uniquement en haut pour les 7èmes)
      - if a note is a seventh or a fourth for the scale, the next note has to be the key
      - if it is a seventh, the following note is the key above (+1 for major or +2 for minor)
      - if it is a fourth, then it can be above (+7) or below (-5)
    */
    void dissonnanceResolution(int key, bool major){//works but only for sevenths note, note for fourths -> to improve
      IntVarArgs temp(pitch.slice(pitch.size() -1, -1)); //reverse the array to enforce subsedience constraint with precedence constraint
      for(int i = 0; i < sevenths.size(); i++){
        precede(*this, temp, sevenths[i] + 1, sevenths[i]); //if there is a seventh, there will eventually be a tonic after it
      }    
    }
    /*
      the maximum interval between two adjacent notes is an octave and is one of the possible values
    */
    void interval(){//constrains intervals to acceptable values, but doesn't prioritize smaller intervals
      for(int i = 0; i < n-1; i++){
        //pitch[i] - pitch[i+1] - z[i] = 0
        IntVarArray temp(*this, 3, -12, 108);
        //initializiation
        /* dom(*this, temp[0], pitch[i]);
        dom(*this, temp[1], pitch[i+1]);
        dom(*this, temp[2], intervals[i]); */

        //linking the temporary variables to the ones they represent
        rel(*this, temp[0], IRT_EQ, pitch[i]);
        rel(*this, temp[1], IRT_EQ, pitch[i+1]);
        rel(*this, temp[2], IRT_EQ, intervals[i]);

        //adding the constraint on the interval between the two notes
        linear(*this, {1, -1, -1}, temp, IRT_EQ, 0);
      } 
    }
    /*
      sur les accords majeurs/mineurs : doubler la fondamentale en priorité, puis la quinte, puis la tierce
    */
    void noteOnChord(){//tested, works
      for(int i = 0; i < chords.size(); i++){//for each chord
        //get the notes that can be played on top of the chord
        vector<int> notes(getAdmissibleNotes(chords[i]));
        for(int j = 0; j < start.size(); j++){//for each note
          if(chordStart[i] == start[j]){//the note is played at the same time as the chord
            const vector<int> v(notes);
            IntSet set(v);
            dom(*this, pitch[j], set);
          }
        }
      }
    }
    /*
    * returns the notes that can be played on top of a certain chords in order of preference
    */
    vector<int> getAdmissibleNotes(vector<int> chord){//tested, works for major and minor, not done yet for dimineshed/augmented
    //get the intervals between the notes
      vector<int> intervals{chord[1] - chord[0], chord[2] - chord[1]};
      if(isInMode(intervals, majorChords)){// if the chord is major
        int inversion = getInversion(intervals, majorChords);
        int fundamental = getFundamental(chord, inversion);
        vector<int> fundamentals(0);
        vector<int> fifth(0);
        vector<int> third(0);
        //get the admissible notes
        getIths(fundamental, true, 6, &fundamentals);
        getIths(fundamental, true, 1, &third);
        getIths(fundamental, true, 3, &fifth);
        //put everything together in a vector in the right order
        fifth.insert(std::end(fifth), std::begin(third), std::end(third));
        fundamentals.insert(std::end(fundamentals), std::begin(fifth), std::end(fifth));
        return fundamentals;
      }
      else if(isInMode(intervals, minorChords)){//if the chord is minor
        int inversion = getInversion(intervals, minorChords);
        int fundamental = getFundamental(chord, inversion);
        vector<int> fundamentals(0);
        vector<int> fifth(0);
        vector<int> third(0);
        //get the admissible notes
        getIths(fundamental, false, 6, &fundamentals);
        getIths(fundamental, false, 1, &third);
        getIths(fundamental, false, 3, &fifth);
        //put everything together in a vector in the right order
        fifth.insert(std::end(fifth), std::begin(third), std::end(third));
        fundamentals.insert(std::end(fundamentals), std::begin(fifth), std::end(fifth));
        return fundamentals;
      }
      else{// to improve to take into account augmented chords, currently they are treated as diminished chords
        int inversion = getInversion(intervals, diminishedChords);
        int fundamental = getFundamental(chord, inversion);
        vector<int> third(0);
        //get the admissible notes
        getIths(fundamental, false, 1, &third);//TODO : adapter, la c'est pour les mineures
        return third;
      }  
    }
    /*
    * returns the fundamental note of the chord
    */
    int getFundamental(vector<int> chord, int inversion){//works
      if(inversion == 0){
        return chord[0];
      }
      else if(inversion == 1){
        return chord[2];
      }
      else{
        return chord[1];
      }
    }
    /*
    * returns true if the chord is in the mode
    */
    bool isInMode(vector<int> chordIntervals, vector<vector<int>> mode){//tested, works fine
      for(int i = 0; i < mode.size(); ++i){
        if(std::equal(mode[i].begin(), mode[i].end(), chordIntervals.begin())){
          return true;
        }
      }
      return false;
    }
    /*
    * returns the inversion of the chord(0 if it is in fundamental order, one if it is the first inversion or 2 if it is the second inversion)
    */
    int getInversion(vector<int> chordIntervals, vector<vector<int>> mode){//tested, works fine
      for(int i = 0; i < mode.size(); i++){
        if(equal(mode[i].begin(), mode[i].end(), chordIntervals.begin())){
          return i;
        }
      }
      return -1;
    }
    /*
      get the ith notes from the scale
      key is the key, major is true if the scale is major, pos is the position with respect to the key (6 if it is the key), vec is the vector to be filled
    */
    void getIths(int key, bool major, int pos, vector<int>* vec){//tested, works
      vector<int> ith(0);
      int note = key;

      int interval = 0;
      if(major){
        for(int i = 0; i <= pos; ++i){
          interval = interval + majorScale[i];
        }
      }
      else{
        for(int i = 0; i <= pos; ++i){
            interval = interval + minorScale[i];
        }
      }
      note = key + interval;
      while(note < 109){
        ith.push_back(note);
        //std::cout << note << "\n";
        note = note + 12;
      }
      note = key - (12 - interval);
      while (note > 20){
        ith.push_back(note);
        //std::cout << note << "\n";
        note = note - 12;
      }
      *vec = ith;
    }
    /*
    * returns true if the value is a ith
    */
    bool inIths(int value, vector<int> vec){//tested
      for(int i = 0; i < fourths.size(); i++){
        if(vec[i] == value){
          return true;
        }
      }
      return false;
    }    
    /*
      the notes are in the tonality specified
    */
    void inTonality(int key, bool major){  //tested, works correctly but we can add the diminished seventh
      vector<int> notes(0);
      int note = key;
      notes.push_back(note);

      if(major){
        for(int i = 0; i < 7; i++){
          scale[i] = majorScale[i];
        }
      }
      else{  //minor
        for(int i = 0; i < 7; i++){
          scale[i] = minorScale[i];
        }
      }
      //only keep the notes above the key in the tonality
      int pos = 0;
      while(note < 108){   //jusque la note la plus haute
        if(pos >= 7){       //quand on arrive à l'octave suivante on remet major/minor à 0
          pos = 0;
        }
        note = note + scale[pos];
        //std::cout << note << "\n";
        notes.push_back(note); //ajouter au domaine possible la valeur suivante
        ++pos;
      }
      //only keep the notes below the key in the tonality
      note = key;
      pos = 0;
      while(note > minimum){
        if (pos >= 7){
          pos = 0;
        }
        note = note - scale[(7 - pos - 1)];
        //std::cout << note << "\n";
        notes.push_back(note);
        ++pos;
      }
      //update le domaine
      const vector<int> notess = notes;
      IntSet set(notess);
      dom(*this, pitch, set);
    }
    /*
      the notes in the melody are not notes in the chord that is played at the same time and are above notes in the chord
    */
    void restrainDomain(){//tested, works correctly but can be loosened if necessary
      int j = 0;
      for(int i = 0; i < chords.size(); i++){//parcourir tous les accords
        while(start[j] < chordStart[i] + chordDuration[i] - 1 && j < n){//pour chaque note jouée en même temps que l'accord
          dom(*this, pitch[j], chords[i][2] + 1, chords[i][2] + 12);  //la note jouée en même temps doit être au dessus de l'accord à maximum une octave
          ++j;
        }
      }
    }
    
    /*
     Constraint to apply everytime a solution is found
     <prev_sol> is the previous solution found by the solver
     <min_variety> is the minimum number of variables that must take a value different from the one they had in the previous solution
    */
    void solutionVariety(vector<int> prev_sol, int min_variety){
      std::cout << "Solution given to the SolutionVariety Constraint : ";
      printIntVector(prev_sol);
      IntVar z(*this, 0, 4-1); // create a variable with domain between 0 and nVariables-1
      IntArgs p_sol(prev_sol); // cast from vector to IntArgs
      count(*this, pitch, p_sol, IRT_EQ, z); // count the number of elements such that pitch[i] = prev_sol[i] and put it in z (pitch is the array of variables)
      rel(*this, z, IRT_LQ, 4-min_variety); // constrain z to be smaller than or equal to the number of elements that are allowed to be similar (n - variety)
      //std::cout << "domain of z between " << z.min() << " and " << z.max() << "\n";
    }
    
    /*
     Constrain method for BAB search
     This is called everytime the solver finds a solution
    */
    virtual void constrain(const Space& _b) {
      const MelodizerLNS& b = static_cast<const MelodizerLNS&>(_b); // cast le noeud de l'arbre de recherche du type space au type du problème
      vector<int> sol(0);
      for(int i = 0; i < pitch.size(); ++i){
        sol.push_back(b.pitch[i].val()); //keep the values of the solution in a vector
      }
      static vector<vector<int>> solutions({});// keep a list of all the previous solutions
      solutions.push_back(sol);
      //std::cout << "current set of solutions \n";
      //printIntVectorVector(solutions);
      for(int i = 0; i < solutions.size(); ++i){
        solutionVariety(solutions[i], 3);// post it for all the previously found solutions
      }
      //solutionVariety(sol, 2);// post the constraint
    }

    /*
      Constructor : 
      chords are given with the notes in ascending order
      the key is given in MIDI notation from C5 to B5(example : middle C = 60)
      if the tonality is major then major is true, else false
    */
    MelodizerLNS(vector<vector<int>> chordSequence, vector<int> chordDurationSequence, vector<int> chordStartSequence, 
                  vector<int> notesDurations, vector<int> notesStarts, int key, bool major) : pitch(*this,4, 1, 3), 
                  intervals(*this, 5, -12, 12), start(notesStarts), 
                  chords(chordSequence), chordStart(chordStartSequence), chordDuration(chordDurationSequence), 
                  n(notesStarts.size()) {  //pour l'instant, 2 mesures, 4 accords, 8 noires

      dom(*this, intervals, admissibleIntervalsBetweenAdjacentNotes);

      //get all notes per type
      getIths(key, major, 0, &seconds);
      getIths(key, major, 1, &thirds);
      getIths(key, major, 2, &fourths);
      getIths(key, major, 3, &fifths);
      getIths(key, major, 4, &sixths);
      getIths(key, major, 5, &sevenths);
      getIths(key, major, 6, &firsts);

      //restrainDomain();
      //inTonality(key, major);
      //noteOnChord();
      //interval();
      //distinct(*this, pitch);
      //dissonnanceResolution(key, major);

      // post branching
      Rnd r1(12U);
      Rnd r2(13U);
      //branch(*this, pitch, INT_VAR_RND(r1), INT_VAL_RND(r2));
      branch(*this, pitch, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }
    // search support
    MelodizerLNS(MelodizerLNS& s) : Space(s) {
      pitch.update(*this, s.pitch);
    }
  
    virtual Space* copy(void) {
      return new MelodizerLNS(*this);
    }
};
// retourne le ratio de notes identiques dans les 2 solutions TESTED, works
double compareSol(vector<int> sol1, vector<int> sol2){
  int same = 0;
  for(int i = 0; i < sol1.size(); ++i){
    if(sol1[i] == sol2[i]){
      ++same;
    }
  }
  return (same*1.0) / sol1.size();
}

// main function
int main(int argc, char* argv[]) {
  //general test to see if constraints work together
  //chords passed as an argument, for now 
/*   vector<int> chord1{55,60,64}; //CM
  vector<int> chord2{57,60,65}; //FM
  vector<int> chord3{59,62,67}; //GM
  vector<int> chord4{55,60,64}; //CM

  vector<vector<int>> chordSeq{chord1, chord2, chord3, chord4};
  vector<int> chordDur{8, 8, 8, 8};
  vector<int> chordStar{0,8,16,24};

  vector<int> inputDurations{8, 6, 2, //mesure 1
                          6, 2, 8};   //mesure 2
  vector<int> inputStarts{0, 8, 14, 16, 22, 24}; */

  //simple test to test if a single constraint works
  vector<int> chord1{55, 60, 64};
  vector<vector<int>> chordSeq{chord1};
  vector<int> chordDur{16};
  vector<int> chordStar{0};

  vector<int> inputDurations{8, 8};
  vector<int> inputStarts{0, 8, 12};

  // create model and search engine
  MelodizerLNS* m = new MelodizerLNS(chordSeq, chordDur, chordStar, inputDurations, inputStarts, 60, true);
  Gecode::Search::Options opts;
  //opts.threads = 0;// use as many threads as possible
  Gecode::Search::TimeStop maxTime(1);
  opts.stop = &maxTime;
  BAB<MelodizerLNS> e(m, opts);
  delete m;
  int nbSol = 0;

  vector<vector<int>> solutions(0);
  // search and print all solutions
  while (MelodizerLNS* s = e.next()){
    IntVarArray sol(s->getPitch());
    vector<int> newsol(0);
    for(int i = 0; i < sol.size(); ++i){//for each variable
      for (IntVarValues j(sol[i]); j(); ++j){// collect its value
        newsol.push_back(j.val()); // add it to the new solution
      }
    }
    solutions.push_back(newsol);
    ++nbSol;
    maxTime.reset();// reset the max limit of time to search for a solution
  }
  //afficher les résultats
  std::cout << endl << "Solutions : " << endl;
  for(int i = 0; i < solutions.size(); ++i){// for each solution
    printIntVector(solutions[i]);
  }
  std::cout << "number of solutions : " << nbSol << "\n";
  return 0;
}