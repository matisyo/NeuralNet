#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */
#include <math.h>       /* exp */
#include <string>
#include <algorithm>
#include <sstream>
#include <map>
#include <vector>
#include <iomanip>


using namespace std;
double vectors_dot_prod2(const float *x, const float *y, int n)
{
    float res = 0.0;
    int i = 0;
    for (; i < n; i++)
    {
        res += x[i] * y[i];
    }
    return res;
}

void print_matrix(float** matrix, int rows, int cols)
{
    for(int i = 0; i < rows; ++i) {

        for(int j = 0; j < cols; ++j) {

            cout << matrix[i][j] << " ";
        }
        cout << endl;
    }
}
void create_matrix(float** matrix, int rows, int cols)
{
    /* initialize random seed: */
    for(int i = 0; i < rows; ++i) {
        for(int j = 0; j < cols; ++j) {
            matrix[i][j] =-5 + static_cast <float> (rand()) /( static_cast <float> (RAND_MAX/(5+5))); ;
        }
    }
}

float func(float z){
    float x = 0;
    x = 1 / (1+exp(z*-1));
    return x;
}
float* get_next_layer(float* layer,float** weights,int alto,int ancho,float* a){


    for(int i = 0; i < alto; ++i){
        a[i] = weights[i][0];
    }

    for(int i = 0; i < alto; ++i){

        for(int j = 1; j < ancho; ++j){
            a[i] = a[i]+weights[i][j]*layer[j-1];

        }
        a[i] = func(a[i]);
   }
    return a;
}

float* calc_gradient(float** weights,float* a,float* delta,int alto,int ancho){


    //RECORDAR QUE ES EL ALTO DE DELTA alto = m+1 ancho = k
    float* b = new float[alto-1];

    for(int j = 0; j < alto-1; ++j){
        b[j] = 1-a[j];
    }

    float* weight_times_delta = new float[alto-1];
    for(int i = 1; i < alto; ++i){
        weight_times_delta[i-1]=0;

        for(int j=0; j <ancho;++j){
            weight_times_delta[i-1] = weight_times_delta[i-1] + weights[j][i]*delta[j];
        }
    }

    for(int j=0; j <alto-1;++j){
        b[j]=weight_times_delta[j]*a[j]*b[j];

    }

    return b;
}

string remove_comillas(string s,int pos){


    string primera_parte, segunda_parte;
    primera_parte = s.substr(0,pos);
    segunda_parte = s.substr(pos+1,s.size());
    int found = segunda_parte.find('"');
    segunda_parte = segunda_parte.substr(found+1,s.size());
    s = primera_parte + segunda_parte;

    return s;

}

string* read_from_file(int k){
    string* lista = new string[k];
    int cont = 0;
    ifstream file;
    file.open("data/categories.csv");
    string line;
    while(!file.eof()){
        getline(file, line);

        lista[cont] = line;
        cont++;
        if(cont == 39){
            break;
        }
    }
    file.close();
    return lista;
}
float* create_Y_vector(string palabra,string* comparador,int k){
    float* vec_y = new float[k];

    for(int i =0; i<k; i++){
        if(palabra == comparador[i])
            vec_y[i] = 1;
        else
            vec_y[i] = 0;
    }
    return vec_y;
}
float* day_parser(float* vec,string palabra,string* dias){

    for(int i =0; i<7; i++){
        if(palabra == dias[i])
            vec[i+27] = 1;
        else
            vec[i+27] = 0;
    }
    return vec;
}

float* district_parser(float* vec,string palabra,string* district){

    for(int i =0; i<10; i++){
        if(palabra == district[i])
            vec[i+34] = 1;
        else
            vec[i+34] = 0;
    }
    return vec;
}

float* time_parser(float* vec, string palabra){

    //HORA
    int hora = (palabra[11]-48)*10 + (palabra[12]-48);

    for(int i = 0; i<12;i++){
        if((hora == i*2) or (hora == i*2+1))
            vec[i] = 1;
        else
            vec[i] = 0;
    }

    //NOCHE Y DIA
    vec[49] = 0;
    vec[48] = 0;
    if ((hora <19) and (hora>6))
        vec[49] = 1;
    else
        vec[48] = 1;

    //AÑO
    int anio = (palabra[2]-48)*10 + (palabra[3]-48);

    for(int i = 12; i<27;i++){
        if(anio == i-12)
            vec[i] = 1;
        else
            vec[i] = 0;
    }
    //ESTACION


    vec[44]=0;//verano
    vec[45]=0;//otoño
    vec[46]=0;//invierno
    vec[47]=0;//primavera
    int m= (palabra[5]-48)*10 + (palabra[6]-48);
    int d= (palabra[8]-48)*10 + (palabra[9]-48);
    if ((m == 1) or (m == 2))
        vec[46] = 1;
    else if ((m== 4) or (m == 5))
        vec[47] = 1;
    else if(( m == 7)or (m == 8))
        vec[44] = 1;
    else if ((m== 10) or (m == 11))
        vec[45] = 1;
    else if (m == 12)
        if (d > 21)
            vec[46] = 1;
        else
            vec[45] = 1;
    else if (m == 3)
        if (d> 21)
            vec[47] = 1;
        else
            vec[46] = 1;
    else if (m== 6)
        if (d > 21)
            vec[44] = 1;
        else
            vec[47] = 1;
    else if (m== 9)
        if (d > 21)
            vec[45] = 1;
        else
            vec[44] = 1;

    return vec;
}
int main()
{


    int k = 39;
    int m= 120;
    int n=50;
    srand (time(NULL));

    //Creo matrices de pesos
    float** weights1 = new float*[m];
    for(int i = 0; i < m; ++i)
        weights1[i] = new float[n+1];

    float** weights2 = new float*[k];
    for(int i = 0; i < k; ++i)
        weights2[i] = new float[m+1];

    //Las randomizo
    create_matrix(weights1,m,n+1);
    create_matrix(weights2,k,m+1);


    //Creo matrices de acumulacion de derivadas
    float** acumulador1 = new float*[m];
    for(int i = 0; i < m; ++i)
        acumulador1[i] = new float[n+1];

    float** acumulador2 = new float*[k];
    for(int i = 0; i <k; ++i)
        acumulador2[i] = new float[m+1];


    for (int i=0;i<m;i++){
        for (int j=0;j<n+1;j++){
            acumulador1[i][j]=0;
        }
    }

    for (int i=0;i<k;i++){
        for (int j=0;j<m+1;j++){
            acumulador2[i][j]=0;
        }
    }


    //Declaro variables de nodos y resultados
    float* my_y_vec= new float[k];
    float* my_x_vec= new float[n];
    float* trainY = new float[k];
    float* a = new float[m];


    //Declaro variables delta
    float* delta3 = new float[k];
    float* delta2 = new float[m];


    //COSAS PARA EL PARSEO
    string* lista_categorias = new string[k];
    lista_categorias = read_from_file(k);
    //REEE CABEZAAA
    string* lista_dias = new string[7];
    lista_dias[0] = "Monday";
    lista_dias[1] = "Tuesday";
    lista_dias[2] = "Wednesday";
    lista_dias[3] = "Thursday";
    lista_dias[4] = "Friday";
    lista_dias[5] = "Saturday";
    lista_dias[6] = "Sunday";

    string* lista_distritos = new string[10];
    lista_distritos[0] = "BAYVIEW" ;
    lista_distritos[1] = "CENTRAL";
    lista_distritos[2] = "INGLESIDE";
    lista_distritos[3] = "MISSION";
    lista_distritos[4] = "NORTHERN";
    lista_distritos[5] = "PARK";
    lista_distritos[6] = "RICHMOND";
    lista_distritos[7] = "SOUTHERN";
    lista_distritos[8] = "TARAVAL";
    lista_distritos[9] = "TENDERLOIN";

    double* vec_promedios = new double[n];
    for(int ele=0;ele<n;ele++){
        vec_promedios[ele] = double(0);
    }
    double* lista_esperanzas = new double[n];
    double total_train=878050;
    lista_esperanzas[0]=double(71038)/total_train;
    lista_esperanzas[1]=double(36310)/total_train;
    lista_esperanzas[2]=double(18500)/total_train;
    lista_esperanzas[3]=double(35181)/total_train;
    lista_esperanzas[4]=double(68455)/total_train;
    lista_esperanzas[5]=double(76179)/total_train;
    lista_esperanzas[6]=double(95079)/total_train;
    lista_esperanzas[7]=double(92482)/total_train;
    lista_esperanzas[8]=double(103690)/total_train;
    lista_esperanzas[9]=double(104579)/total_train;
    lista_esperanzas[10]=double(88355)/total_train;
    lista_esperanzas[11]=double(88201)/total_train;
    lista_esperanzas[12]=double(0)/total_train;
    lista_esperanzas[13]=double(0)/total_train;
    lista_esperanzas[14]=double(0)/total_train;
    lista_esperanzas[15]=double(73902)/total_train;
    lista_esperanzas[16]=double(73422)/total_train;
    lista_esperanzas[17]=double(70779)/total_train;
    lista_esperanzas[18]=double(69909)/total_train;
    lista_esperanzas[19]=double(68015)/total_train;
    lista_esperanzas[20]=double(70174)/total_train;
    lista_esperanzas[21]=double(69000)/total_train;
    lista_esperanzas[22]=double(66542)/total_train;
    lista_esperanzas[23]=double(66619)/total_train;
    lista_esperanzas[24]=double(71731)/total_train;
    lista_esperanzas[25]=double(75606)/total_train;
    lista_esperanzas[26]=double(74766)/total_train;
    lista_esperanzas[27]=double(121584)/total_train;
    lista_esperanzas[28]=double(124965)/total_train;
    lista_esperanzas[29]=double(129211)/total_train;
    lista_esperanzas[30]=double(125038)/total_train;
    lista_esperanzas[31]=double(133734)/total_train;
    lista_esperanzas[32]=double(126810)/total_train;
    lista_esperanzas[33]=double(116707)/total_train;
    lista_esperanzas[34]=double(89431)/total_train;
    lista_esperanzas[35]=double(85460)/total_train;
    lista_esperanzas[36]=double(78845)/total_train;
    lista_esperanzas[37]=double(119908)/total_train;
    lista_esperanzas[38]=double(105296)/total_train;
    lista_esperanzas[39]=double(49313)/total_train;
    lista_esperanzas[40]=double(45209)/total_train;
    lista_esperanzas[41]=double(157182)/total_train;
    lista_esperanzas[42]=double(65596)/total_train;
    lista_esperanzas[43]=double(81809)/total_train;
    lista_esperanzas[44]=double(223394)/total_train;
    lista_esperanzas[45]=double(206626)/total_train;
    lista_esperanzas[46]=double(225905)/total_train;
    lista_esperanzas[47]=double(222124)/total_train;
    lista_esperanzas[48]=double(365013)/total_train;
    lista_esperanzas[49]=double(513037)/total_train;

    int cont = 0;
    int contador_secundario=10000;
    string line;
    string line_aux;
    float alpha = 0.01;
    float gamma = 0.0001;

    bool found_alpha = false;
while(!found_alpha){
    found_alpha = true;
    create_matrix(weights1,m,n+1);
    create_matrix(weights2,k,m+1);
for(int epoch=0;epoch<1;epoch++){
    cont = 0;
    contador_secundario=10000;
    ifstream file;
    file.open("data/train_random.csv");
    getline(file, line);


    while(!file.eof()){
        getline(file, line);
        int found = line.find('"');

        while(found>0){
            line = remove_comillas(line,found);
            found = line.find('"');
        }

        //TIME
        found = line.find(',');
        line_aux = line.substr (0,found);
        line = line.substr(found+1,line.size());
        my_x_vec = time_parser(my_x_vec,line_aux);
        //cout<<line_aux<<" ";

        //CATEGORY
        found = line.find(',');
        line_aux = line.substr (0,found);
        //cout<<line_aux<<" ";

        my_y_vec = create_Y_vector(line_aux,lista_categorias,k);

        //ignoro la descripcion
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        //

        //Day Of Week
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        my_x_vec = day_parser(my_x_vec,line_aux,lista_dias);
        //cout<<line_aux<<" ";

        //Pd District
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        my_x_vec = district_parser(my_x_vec,line_aux,lista_distritos);
        //cout<<line_aux<<" ";

        // IGNORE RESOLUTION
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        //

        //Address
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        //cout<<line_aux<<" ";


        //SOLO SI ANDA BIEN LO AGREGO
        for(int ele=0;ele<n;ele++){
            my_x_vec[ele]=my_x_vec[ele]-lista_esperanzas[ele];

        }

        //FORWARD PROPAGATOIN
        a = get_next_layer(my_x_vec,weights1,m,n+1,a);
        trainY = get_next_layer(a,weights2,k,m+1,trainY);

        //BACK PROPAGATION
        for(int i = 0; i < k; ++i){
            delta3[i] = trainY[i]-my_y_vec[i];
        }

        delta2 = calc_gradient(weights2,a,delta3,m+1,k);



        //Refresh Acumulador

        for (int i=0;i<k;i++)
            weights2[i][0]=weights2[i][0]-alpha*delta3[i];
        for(int i =0;i<k;i++){
            for(int j=0;j<m;j++){
                weights2[i][j+1]=(1-alpha*gamma)*weights2[i][j+1]-alpha*delta3[i]*a[j];
            }
        }

        for (int i=0;i<m;i++)
            weights1[i][0]=weights1[i][0]-alpha*delta2[i];
        for(int i =0;i<m;i++){
            for(int j=0;j<n;j++){
                weights1[i][j+1]=(1-alpha*gamma)*weights1[i][j+1]-alpha*delta2[i]*my_x_vec[j];
            }
        }




        if (cont ==contador_secundario){
                cout<<contador_secundario<<endl;
                contador_secundario = contador_secundario+10000;
                my_x_vec = time_parser(my_x_vec,"2015-05-10 23:59:00");
                my_x_vec = district_parser(my_x_vec,"BAYVIEW",lista_distritos);
                my_x_vec = day_parser(my_x_vec,"Sunday",lista_dias);
                my_x_vec = time_parser(my_x_vec,line_aux);

                //FORWARD PROPAGATOIN
                a = get_next_layer(my_x_vec,weights1,m,n+1,a);
                trainY = get_next_layer(a,weights2,k,m+1,trainY);

                cout<<alpha<<" "<<trainY[1]<<endl;


                /*if(trainY[1]<0.001){
                    cout<<alpha<<" "<<trainY[1]<<endl;
                    alpha = alpha -0.0001;
                    found_alpha = false;
                    break;
                }
                if (cont>870000){

                    if (cont>860000)
                    for(int parametro=0;parametro<k;parametro++)
                        if(trainY[parametro]>0.5){
                            found_alpha = false;
                            cout<<alpha<<" "<<trainY[parametro]<<endl;
                        }

                    if(found_alpha){
                    alpha = alpha -0.0001;

                    break;
                    }

                }*/

        }
        cont++;
    }
     cout<<cont<<endl;


    file.close();
}

}

    cout<<"Pesos entrenados"<<endl;

    //Abro archivo de resultado

    ofstream res;
    res.open("data/resultado.csv");

    //Abro archivo de test
    ifstream file_test;
    file_test.open("data/test.csv");

    //Add title
    res<<"Id";
    for(int i = 0;i<39;i++){
            res << ","<<lista_categorias[i];
        }
    res << endl;

    cont = 0;
    contador_secundario = 10000;
    getline(file_test, line);

    while(!file_test.eof()){

        getline(file_test, line);

        int found = line.find('"');

        while(found>0){
            line = remove_comillas(line,found);
            found = line.find('"');
        }

        //ID
        found = line.find(',');
        line_aux = line.substr (0,found);
        my_x_vec = time_parser(my_x_vec,line_aux);
        line = line.substr(found+1,line.size());
        //cout<<line_aux<<" ";

        //TIME
        found = line.find(',');
        line_aux = line.substr (0,found);
        line = line.substr(found+1,line.size());
        my_x_vec = time_parser(my_x_vec,line_aux);
        //cout<<line_aux<<" ";

        //Day Of Week

        found = line.find(',');
        line_aux = line.substr (0,found);
        my_x_vec = day_parser(my_x_vec,line_aux,lista_dias);
        //cout<<line_aux<<" u ";

        //Pd District
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        my_x_vec = district_parser(my_x_vec,line_aux,lista_distritos);
        //cout<<line_aux<<" ";

        //Address
        line = line.substr(found+1,line.size());
        found = line.find(',');
        line_aux = line.substr (0,found);
        //cout<<line_aux<<" ";


         for(int ele=0;ele<n;ele++){
            my_x_vec[ele]=my_x_vec[ele]-lista_esperanzas[ele];

        }

       //FORWARD PROPAGATOIN
        a = get_next_layer(my_x_vec,weights1,m,n+1,a);
        trainY = get_next_layer(a,weights2,k,m+1,trainY);



        if (cont == 884262){
            break;
        }
        //Actualizo archivo
        res << cont;
        cont++;
        for(int i = 0;i<39;i++){
            res << ","<<trainY[i];
        }
        res << endl;

        if (cont == contador_secundario){
            cout<<contador_secundario<<endl;
            contador_secundario= contador_secundario+10000;
        }


    }
    for(int i = 0;i<39;i++){
            cout << ","<<trainY[i];
        }
    file_test.close();

    res.close();

    return 0;
}
