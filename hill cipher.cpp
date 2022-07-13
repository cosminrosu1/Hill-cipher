#include<iostream>
#include<string>
#include<math.h>
#include<time.h>
#define n 3 //numarul de linii si coloane al matricei

//algoritmul functioneaza pentru matrici cu 2 sau mai multe linii si coloane
//algoritmul functioneaza pentru literele mici ale alfabetului limbii engleze

using namespace std;

//functie pentru modulo
int modulo(int a, int b)
{
	int r = a % b;
	return r < 0 ? r + b : r;
}

//functie care returneaza cmmdc a doua numere
int cmmdc(int a, int b) {
	if (a < 0) {
		a = a * (-1);
	}
	while (a != b) {
		if (a > b) {
			a = a - b;
		}
		else {
			b = b - a;
		}
	}
	return a;
}

//functie care returneaza determinantul unei matrici; pentru matrici de ordin >2 se foloseste regula lui Sarrus
int determinant(int matrix[][n]) {
	int det = 0;

	if (n == 2) {
		det = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
		return det;
	}

	int matrix_det[n + 2][n];
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			matrix_det[i][j] = matrix[i][j];
		}
	}

	int k = 0;
	for (int i = n; i < n + 2; i++) {
		for (int j = 0; j < n; j++) {
			matrix_det[i][j] = matrix_det[k][j];
		}
		k++;
	}

	int aux, sum;
	for (int i = 0; i < n; i++) {
		sum = 1;
		aux = i;
		for (int j = 0; j < n; j++) {
			sum = sum * matrix_det[aux][j];
			aux++;
		}
		det = det + sum;
	}

	for (int i = 0; i < n; i++) {
		sum = 1;
		aux = i;
		for (int j = n - 1; j >= 0; j--) {
			sum = sum * matrix_det[aux][j];
			aux++;
		}
		det = det - sum;
	}

	return det;
}

//functie care face inmultirea unei matrici de ordin n cu o matrice cu n linii si o coloana
void inmultire(int matrix[][n], int matrix1[][1], int matrice_rezultat[][1]) {
	int sum, aux;

	for (int i = 0; i < n; i++) {
		sum = 0;
		aux = 0;
		for (int j = 0; j < n; j++) {
			sum = sum + matrix[i][aux] * matrix1[aux][0];
			aux++;
		}
		matrice_rezultat[i][0] = sum;
	}
}

//functie care face adunarea a doua matrici cu n linii si o coloana
void adunare(int matrix[][1], int matrix1[][1]) {
	for (int i = 0; i < n; i++) {
		matrix[i][0] = matrix[i][0] + matrix1[i][0];
	}
}

//functie care face scaderea a doua matrici cu n linii si o coloana
void scadere(int matrix[][1], int matrix1[][1]) {
	for (int i = 0; i < n; i++) {
		matrix[i][0] = matrix[i][0] - matrix1[i][0];
	}
}

//functie care elimina o linie si o coloana dintr-o matrice de ordin n
void elim_lin_col(int matrix[][n], int linia, int coloana, int matrice_rezultat[][n - 1]) {
	int matrice_aux[n][n];

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			matrice_aux[i][j] = matrix[i][j];
		}
	}

	for (int i = linia; i < n - 1; i++) {
		for (int j = 0; j < n; j++) {
			matrice_aux[i][j] = matrice_aux[i + 1][j];
		}
	}

	for (int i = 0; i < n - 1; i++) {
		for (int j = coloana; j < n - 1; j++) {
			matrice_aux[i][j] = matrice_aux[i][j + 1];
		}
	}

	for (int i = 0; i < n - 1; i++) {
		for (int j = 0; j < n - 1; j++) {
			matrice_rezultat[i][j] = matrice_aux[i][j];
		}
	}
}

//functie care face inversa unei matrici de ordin n
void inversa(int matrix[][n], int matrice_rezultat[][n]) {
	int matrix_inv[n][n];
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			matrix_inv[j][i] = matrix[i][j];
		}
	}

	int matrix_adj[n][n];
	if (n == 2) { //atunci cand ordinul matricei este 2
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (i == j && i == 0) {
					matrix_adj[i][j] = (pow((-1), (i + j))) * matrix_inv[1][1];
					matrix_adj[i][j] = modulo(matrix_adj[i][j], 26);
				}
				else if (i == j && i == 1) {
					matrix_adj[i][j] = (pow((-1), (i + j))) * matrix_inv[0][0];
					matrix_adj[i][j] = modulo(matrix_adj[i][j], 26);
				}
				else {
					matrix_adj[i][j] = (pow((-1), (i + j))) * matrix_inv[j][i];
					matrix_adj[i][j] = modulo(matrix_adj[i][j], 26);
				}

			}
		}
	}
	else { //atunci cand ordinul matricei este mai mare decat 2
		int matrix_aux[n - 1][n - 1];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) { 
				elim_lin_col(matrix_inv, i, j, matrix_aux);
				matrix_adj[i][j] = (pow((-1), (i + j))) * (matrix_aux[0][0] * matrix_aux[1][1] - matrix_aux[0][1] * matrix_aux[1][0]);
				matrix_adj[i][j] = modulo(matrix_adj[i][j], 26);
			}
		}
	}

	int aux = 1;
	while (modulo((determinant(matrix) * aux), 26) != 1) {
		aux++;
	}

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			matrix_adj[i][j] = modulo((matrix_adj[i][j] * aux), 26);
		}
	}

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			matrice_rezultat[i][j] = matrix_adj[i][j];
		}
	}
}

//functie de criptare care primeste ca parametii matricea, mesajul in clar grupat in cate n caractere, vectorul
string criptare(int matrix[][n], string mesaj_clar_grouped, int vector[]) {
	string mesaj_criptat = mesaj_clar_grouped;

	int vector_matr[n][1];
	for (int i = 0; i < n; i++) {
		vector_matr[i][0] = vector[i];
	}

	int aux = 0;
	int k;
	int string_matr[n][1], rezultat_matr[n][1];
	int q = -1;
	for (int i = 0; i < mesaj_clar_grouped.size(); i++) {
		k = mesaj_clar_grouped[i] - 97;
		string_matr[aux][0] = k;
		aux++;
		if (aux == n) {
			aux = 0;
			i++;

			inmultire(matrix, string_matr, rezultat_matr);
			adunare(rezultat_matr, vector_matr);

			for (int j = 0; j < n; j++) {
				q++;
				mesaj_criptat[q] = modulo(rezultat_matr[j][0], 26) + 97;
			}
			q++;
			mesaj_criptat[q] = ' ';
		}
	}
	mesaj_criptat[q] = NULL;

	return mesaj_criptat;
}

//functie de decriptare care primeste ca parametrii matricea, mesajul criptat, vectorul
string decriptare(int matrix[][n], string mesaj_criptat, int vector[]) {
	string mesaj_decriptat;
	mesaj_decriptat.resize(100);

	int vector_matr[n][1];
	for (int i = 0; i < n; i++) {
		vector_matr[i][0] = vector[i];
	}

	int aux = 0;
	int k;
	int string_matr[n][1], rezultat_matr[n][1];
	int q = -1;
	for (int i = 0; i < mesaj_criptat.size(); i++) {
		k = mesaj_criptat[i] - 97;
		string_matr[aux][0] = k;
		aux++;
		if (aux == n) {
			aux = 0;
			i++;

			scadere(string_matr, vector_matr);
			inmultire(matrix, string_matr, rezultat_matr);

			for (int j = 0; j < n; j++) {
				q++;
				mesaj_decriptat[q] = modulo(rezultat_matr[j][0], 26) + 97;
			}
		}
	}

	mesaj_decriptat.resize(q + 1);
	return mesaj_decriptat;
}

int main() {
	string mesaj_clar;
	cout << "Mesaj in clar: ";
	getline(cin, mesaj_clar);

	string mesaj_clar_grouped;
	mesaj_clar_grouped.resize(100);

	//grupare mesaj in clar in cate n caractere
	int count = 0, k = 0;
	for (int i = 0; i < mesaj_clar.size(); i++) {
		if (mesaj_clar[i] != ' ') {
			mesaj_clar_grouped[k] = mesaj_clar[i];
			count++;
			if (count == n) {
				k++;
				mesaj_clar_grouped[k] = ' ';
				count = 0;
			}
			k++;
		}
	}

	int num = 0;
	for (int i = 0; i < mesaj_clar.size(); i++) {
		if (mesaj_clar[i] != ' ') {
			num++;
		}
	}

	//daca in ultima grupare de caractere nu sunt n caractere, atunci se adauga litera 'a' pana se fac n caractere
	while (num % n != 0) {
		mesaj_clar_grouped[k] = 'a';
		k++;
		num++;
	}

	mesaj_clar_grouped[k + 1] = NULL;
	mesaj_clar_grouped.resize(k);

	int matrix[n][n], vector[n];

	cout << endl;

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			cout << "matrix[" << i << "][" << j << "]= ";
			cin >> matrix[i][j];
		}
	}

	cout << endl;

	//matricea poate fi folosita doar daca determinanul sau este 0 si cmmdc-ul cu 26 este diferit de 1
	if (determinant(matrix) == 0 || cmmdc(determinant(matrix), 26) != 1) {
		cout << "Matricea nu poate fi folosita!" << endl;
		return 0;
	}
	else {
		cout << "Matricea poate fi folosita!" << endl;
	}

	//elementele din vector sunt generate aleator intre 1 si 100
	srand(time(NULL));

	for (int i = 0; i < n; i++) {
		vector[i] = rand() % 100 + 1;
	}
	cout << endl;
	for (int i = 0; i < n; i++) {
		cout << "vector[" << i << "]= " << vector[i] << endl;
	}

	cout << endl;
	string mesaj_criptat;
	mesaj_criptat.resize(mesaj_clar_grouped.size());
	mesaj_criptat = criptare(matrix, mesaj_clar_grouped, vector);
	cout << "Mesaj criptat: " << mesaj_criptat << endl;

	int matrix_adj[n][n];
	inversa(matrix, matrix_adj);

	string mesaj_decriptat;
	mesaj_decriptat.resize(mesaj_criptat.size());
	mesaj_decriptat = decriptare(matrix_adj, mesaj_criptat, vector);
	cout << "Mesaj decriptat: " << mesaj_decriptat;

	return 0;
}
