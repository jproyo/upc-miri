#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define EOS '\0'

#define ASIZE 4
#define WORD_SIZE 64

long readfile(char *filename, char *text[])
{
	FILE *fin;
	long len;
	int i = 0;
	char c;
	
	if (!(fin = fopen(filename, "r"))) {
		fprintf (stderr, "I can't open the file: %s\n", filename);
		exit(1);
	}
	// llegim el nombre de caracters que hi ha al fitxer
	fseek(fin, 0, SEEK_END);
	len = ftell(fin);
	fseek(fin, 0, SEEK_SET);
	// demanem memoria per la variable text
	*text = malloc(len + 1);
        // bucle per llegir el fitxer
	while ((c = (fgetc(fin))) != EOF) {
	  if ((c=='a')||(c=='c')||(c=='g')||(c=='t')||(c=='A')||(c=='C')||(c=='G')||(c=='T')){
	    (*text)[i] = c;
	    //printf("text[%d] = %c\n", i, (*text)[i]);
	    i++;
	  }
	}
	fclose(fin);
	return i;
}

void BNDM(char *x, int m, char *y, int n) {
  int B[ASIZE];
  int i, j, s, d, last;
  if (m > WORD_SIZE)
    printf("error BNDM");

  /* Pre processing */
  memset(B,0,ASIZE*sizeof(int));
  s=1;
  for (i=m-1; i>=0; i--){
    B[x[i]] |= s;
    s <<= 1;
  }

  /* Searching phase */
  j=0;
  while (j <= n-m){
    i=m-1; last=m;
    d = ~0;
    while (i>=0 && d!=0) {
      d &= B[y[j+i]];
      i--;
      if (d != 0){
    if (i >= 0)
      last = i+1;
    // else 
    // printf("%d ",j);
       }
       d <<= 1;
     }
     j += last;
   }
}


int main(int argc, char *argv[])
{
        char pattern[256];
	char filename[256];
	char *text;
	int  n;
	clock_t initemps;

	sprintf(pattern,"%s",argv[2]);
	sprintf(filename,"%s",argv[1]);
	    printf("pattern: %s \n",pattern);
	
	n = readfile(filename, &text);
	//	printf("text: %s \n",text);
	//printf("n=%d\n",n);
	initemps=clock();
	BNDM(pattern,strlen(pattern),text,n);
	printf( "Nombre de segons: %f s\n", (clock()-initemps)/(double)CLOCKS_PER_SEC );
	free(text);
        return 0;
}
