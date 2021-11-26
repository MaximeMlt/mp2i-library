bool has(int* t, int e, int taille){
    for (int i = 0; i<taille; i++){
        if (t[i]==e) return true;
        
    }
    return false;
}

int aux(int* t, int e, int i, int j){
    if (t[j] < t[i]) return 1;
    else {
        int m = ((i + j) / 2);
        if (t[m]==t[e]) return 0;
        else if (t[m]>t[e]) aux(t, e, m+1, j);
        else aux(t, e, i, m-1);
        }
    return 1;
        
    }

bool dichotomie(int* t, int e){
    if (aux(t, e, 0,(sizeof(t)/sizeof(int))) == 0 ) return true;
    else return false;
    
}

int tab[]={1,2,3,4,5,6,7,8,9};
dichotomie(tab, 2)
  
void swap(int* tab, int i, int j){
    int tmp = tab[i];
    tab[i] = tab[j];
    tab[j] = tmp;
}
