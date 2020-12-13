
#include <cstdlib>
#include <iostream>
#include <vector>
#include <utility>
#include <random>
#include <time.h>
#include <iostream>
#include <fstream>

#define NUM_EXPERIMENTS 100
#define TIME_STEPS 100000
#define N_0 10000
#define M_0 1
#define TYPE 2 //TYPE 1= COMPLETE ,TYPE 2=RING

#define MODEL 3

int results_t1[TIME_STEPS][NUM_EXPERIMENTS];
int results_t10[TIME_STEPS][NUM_EXPERIMENTS];
int results_t100[TIME_STEPS][NUM_EXPERIMENTS];
int results_t1000[TIME_STEPS][NUM_EXPERIMENTS];

int avaraged_results_t1[TIME_STEPS];
int avaraged_results_t10[TIME_STEPS];
int avaraged_results_t100[TIME_STEPS];
int avaraged_results_t1000[TIME_STEPS];

using namespace std;

class Node{

    private:   
        int id;
        vector<int> out_connections; 
        
    public:    
        
        //Constructor
        Node( int id){ 
            this->id = id;
        }
        
        //Getter
        vector<int>* getOutConnections(){
        
            return &(this->out_connections);
        
        }
        
        //Getter
        int getId(){
        
            return this->id;
        
        }
        
        // METHODS //
        
        //Adds that node as a connection
        void addOutConnection(int id){ // TODO: it would be nice if they were ordered...
            this->out_connections.push_back(id);
        }
        
        //Returns the node degree
        int getDegree(){
        
            return this->out_connections.size();
        
        }
        
        //Returns "true" if its connected to node v, "false" otherwise
        bool isConnected(int v){
        
            bool is_connected= false;
            
            for(int i=0;i< this->getDegree();i++){
            
                if(this->out_connections.at(i)==v){
                    is_connected= true;
                    break;
                }
            
            }
            
            return is_connected;
        
        }
        
        
    
        


};

class Stub{
    
    public:
        int one;
        int two;
};

class Graph{

    private:
        vector<Node *> nodes;
        int number_of_nodes;
        vector<Stub *> stubs;
       
        std::default_random_engine generator; 
        
        
    public:    
        
        //Constructor
        Graph(){
        
            this->number_of_nodes=0;
            generator.seed(time(NULL));
            
        }
        
        //Getter
        int getNumberOfNodes(){
            
            return this->number_of_nodes;
        
        }
        
        //Getter
        vector<Node *>* getNodes(){
        
            return &(this->nodes);
        
        }
        
        //Getter
        vector<Stub *>* getStubs(){
        
            return &(this->stubs);
        
        }
        
        
        
        
        // METHODS //
        
        //
        Node * getNode(int id){
            
            return this->nodes.at(id); // trick because node with id "i" is in position i
            /*
            Node * node_to_return = NULL;
            
            for(int i=0;i<number_of_nodes;i++){
            
                if(nodes.at(i)->getId()==id){
                    
                    node_to_return =  nodes.at(i);   
                    break;
                }
            
            }
            
            if(node_to_return == NULL) cout << "Warning!!!!" << endl;
        
            return node_to_return; 
             */
        }
        
        //Adds node with id "id"
        void addNode(int id){
        
            Node * n = new Node(id);
            this->nodes.push_back(n);
            number_of_nodes++;
            
        }
        
        //Adds and edge between these nodes u and v
        void addEdge(int u, int v){
        
            if(!getNode(u)->isConnected(v)){
            
                getNode(u)->addOutConnection(v);
                getNode(v)->addOutConnection(u);

                Stub * s1 = new Stub();
                s1->one=u;
                s1->two=v;

                Stub * s2 = new Stub();
                s2->one=v;
                s2->two=u;

                stubs.push_back(s1);
                stubs.push_back(s2);
            
            }
        
        }
        
        /*
        //Return "true" if u and v are connected, "false" otherwise.
        bool isConnected(int u, int v){
        
            bool is_connected= false;
            
            if(getNode(u)->isConnected(v)){
            
                is_connected=true;
            }
            
            return is_connected;
        
        }
        */

        //
        void fillDestinationId(vector <int> * destination_id, int m_0, int my_id){
        
            if(MODEL==1 || MODEL==3){
            
                std::uniform_int_distribution<int> distribution(0,this->stubs.size()-1);

                for(int i=0;i<m_0;i++){ 

                    bool next = false;
                    int dest_id=-1;

                    while(!next){
                        int number = distribution(generator);
                        dest_id = this->stubs.at(number)->one;

                        if(!containsID(destination_id,dest_id) && dest_id!=my_id){ // to avoid multiedges (loops are never going to occur right?)

                            next =true;
                        }
                    }
                    destination_id->push_back(dest_id);

                }
            
            }else if(MODEL ==2){
            
                std::uniform_int_distribution<int> distribution(0,this->number_of_nodes-1);

                for(int i=0;i<m_0;i++){ 

                    bool next = false;
                    int dest_id=-1;

                    while(!next){
                        
                        dest_id = distribution(generator);

                        if(!containsID(destination_id,dest_id) && dest_id!=my_id){ // to avoid multiedges (loops are never going to occur right?)

                            next =true;
                        }
                    }
                    destination_id->push_back(dest_id);

                }
            
            
            }
        
        }
        
        //
        bool containsID(vector <int> * destination_id, int dest_id){
        
            bool contained=false;
            
            for(int i=0;i<destination_id->size();i++){
            
                if(destination_id->at(i)==dest_id){
                
                    contained=true;
                    break;
                }
            
            }
            
            return contained;
        
        }
        
        //
        int highestDegree(){
            
            int highestDegree=0;
            
            for(int i=0;i<this->number_of_nodes;i++){
            
                if(this->nodes.at(i)->getDegree()>highestDegree){
                
                    highestDegree=this->nodes.at(i)->getDegree();
                }
            
            }
            
            return highestDegree;
        
        }
        
        //
        int lowestDegree(){
            
            int lowestDegree=0;
            
            while(numberOfNodesWithDegree(lowestDegree)==0){
            
                lowestDegree++;
            }
            
            return lowestDegree;
        
        }
        
        //
        int numberOfNodesWithDegree(int degree){
        
            int number_of_nodes=0;
        
            for(int i=0;i<this->number_of_nodes;i++){
            
                if(this->nodes.at(i)->getDegree()==degree){
                
                    number_of_nodes++;
                }
            
            }
            
            return number_of_nodes;
        }
        
        
};

class Model{

    private:
        Graph barabasi;
        int n_0;
        int m_0;
        std::default_random_engine generator_model; 
    
    public:
        //Constructor
        Model(int n_0,int m_0,int type){ 

            this->n_0=n_0;
            this->m_0=m_0;
            generator_model.seed(time(NULL));
            
            for(int i=0;i<n_0;i++){

                barabasi.addNode(i);
            }

            
            if(type==1){
            
                //full connected graph...
                for(int i=0;i<n_0;i++){

                    for(int j=0;j<n_0;j++){

                        if(i!=j){
                            barabasi.addEdge(i,j);
                        }
                    }
                }
            
            }else if(type==2){
            
                //ring
                for(int i=0;i<n_0-1;i++){

                    barabasi.addEdge(i,i+1);
                     
                }
                
                barabasi.addEdge(n_0-1,0);
            
            
            }
            

            
        }
        
        Graph * getGraph(){
        
            return &barabasi;
        
        
        }

        void run(int units_of_time,int id_exp){
            

            
            for(int t=0;t<units_of_time;t++){

                if(MODEL==1 || MODEL==2){
                
                    int new_id = n_0+t;

                    barabasi.addNode(new_id); // Adding 1 vertex

                    vector<int>  destination_id;
                    barabasi.fillDestinationId(&destination_id,m_0,new_id);

                    for(int i=0;i<m_0;i++){

                        barabasi.addEdge(new_id,destination_id.at(i));
                    }




                    results_t1[t][id_exp] = barabasi.getNode(n_0)->getDegree();

                    if(t>=9){
                        results_t10[t][id_exp] = barabasi.getNode(n_0+9)->getDegree();
                    }

                    if(t>=99){
                        results_t100[t][id_exp] = barabasi.getNode(n_0+99)->getDegree();
                    }

                    if(t>=999){ 
                        results_t1000[t][id_exp] = barabasi.getNode(n_0+999)->getDegree();
                    }
                
                }else if(MODEL==3){
                
                    std::uniform_int_distribution<int> distribution(0,this->barabasi.getNumberOfNodes()-1);
                    int vertex_chosen = distribution(generator_model); 
                    
                    vector<int>  destination_id;
                    barabasi.fillDestinationId(&destination_id,m_0,vertex_chosen);

                    for(int i=0;i<m_0;i++){

                        barabasi.addEdge(vertex_chosen,destination_id.at(i));
                    }




                    results_t1[t][id_exp] = barabasi.getNode(0)->getDegree(); // don't care wich...!
                
                
                
                
                }
                
                
            }
            

            
            

        }
        
        void writeDegreeDistributionAtThisTime(string file_name){
        
            int number_of_nodes = this->barabasi.getNumberOfNodes();
            
            ofstream myfile;
            myfile.open (file_name);
            
            int highestDegree = this->barabasi.highestDegree();
            int lowestDegree=this->barabasi.lowestDegree();
             
            for(int i=lowestDegree;i<=highestDegree;i++){
            
                myfile << i;
                myfile << " ";
                myfile << (this->barabasi.numberOfNodesWithDegree(i))/*/(double)number_of_nodes*/;
                myfile << "\n";
            
            }
            
            myfile.close();
            
        
        
        }
        
        void writeDegreeOfEachNode(string file_name){
        
            int number_of_nodes = this->barabasi.getNumberOfNodes();
            
            ofstream myfile;
            myfile.open (file_name);
            
            for(int i=0;i<number_of_nodes;i++){
             
                myfile << (this->barabasi.getNode(i)->getDegree());
                myfile << "\n";
            
            }
            
            myfile.close();
            
        
        
        }


};




int main(int argc, char** argv) {

    
    
    // BORRAR...
    /*
    Graph graph; //undirected graph... 

    for(int i=0;i<10;i++){
        graph.addNode(i);
    }
    cout << graph.getNode(2)->getDegree() << endl; 
    cout << graph.getNode(5)->getDegree() << endl; 
    cout << "Okay" << endl; 
    
    graph.addEdge(5,2);
    graph.addEdge(2,5);
    
    cout << graph.getNode(2)->getDegree() << endl; 
    cout << graph.getNode(5)->getDegree() << endl; 
    cout << "Okay" << endl; 
    
    cout << graph.getStubs()->at(0)->one;
    cout << graph.getStubs()->at(0)->two;
    cout << graph.getStubs()->at(1)->one;
    cout << graph.getStubs()->at(1)->two;
    */
    
    // FI BORRAR...
    
    
    
    Model m1(N_0,M_0,TYPE);      
    
    m1.run(TIME_STEPS,0);   
    cout << "EXPERIMENT NUMBER: " << 0 << endl; 
    
    string file_name1="data/degree_distribution";
    string file_name2="data/degree_of_each_node";
    
    m1.writeDegreeDistributionAtThisTime(file_name1.append(to_string(MODEL)));
    m1.writeDegreeOfEachNode(file_name2.append(to_string(MODEL))); 
    
    
    for(int exp=1; exp<NUM_EXPERIMENTS;exp++){
    
        Model * m;
        m= new Model(N_0,M_0,TYPE);
        
        m->run(TIME_STEPS,exp); 
        cout << "EXPERIMENT NUMBER: " << exp << endl;
        
    }
    
      
    
    
    
    
    //averaging... 
    for(int step=999; step<TIME_STEPS;step++){ // Start in step=999 because...
    
        avaraged_results_t1[step]=0;
        avaraged_results_t10[step]=0;
        avaraged_results_t100[step]=0;
        avaraged_results_t1000[step]=0;
        
        for(int num_exp=0; num_exp< NUM_EXPERIMENTS; num_exp++){
        
            avaraged_results_t1[step]= avaraged_results_t1[step]+results_t1[step][num_exp];
            avaraged_results_t10[step]= avaraged_results_t10[step]+results_t10[step][num_exp];
            avaraged_results_t100[step]= avaraged_results_t100[step]+results_t100[step][num_exp];
            avaraged_results_t1000[step]= avaraged_results_t1000[step]+results_t1000[step][num_exp];
            
        }
        
        avaraged_results_t1[step]= avaraged_results_t1[step]/NUM_EXPERIMENTS;
        avaraged_results_t10[step]= avaraged_results_t10[step]/NUM_EXPERIMENTS;
        avaraged_results_t100[step]= avaraged_results_t100[step]/NUM_EXPERIMENTS;
        avaraged_results_t1000[step]= avaraged_results_t1000[step]/NUM_EXPERIMENTS;
    }
    
    
    
    
    ofstream myfile,myfile2,myfile3,myfile4;
    
    string file_name3="data/degree_over_time_1";
    string file_name4="data/degree_over_time_10";
    string file_name5="data/degree_over_time_100";
    string file_name6="data/degree_over_time_1000";
    
    myfile.open  (file_name3.append(to_string(MODEL)));
    myfile2.open (file_name4.append(to_string(MODEL)));
    myfile3.open (file_name5.append(to_string(MODEL)));
    myfile4.open (file_name6.append(to_string(MODEL)));

    for(int step=999; step<TIME_STEPS;step++){ // Start in step=1000 because...
    
        myfile  << (step+1) << " " << avaraged_results_t1[step] << "\n";
        myfile2 << (step+1) << " " << avaraged_results_t10[step] << "\n";
        myfile3 << (step+1) << " " << avaraged_results_t100[step] << "\n";
        myfile4 << (step+1) << " " << avaraged_results_t1000[step] << "\n";
        
        
    }

    myfile.close();
    myfile2.close();
    myfile3.close();
    myfile4.close();
            
    
     

  

    
    return 0;
}

