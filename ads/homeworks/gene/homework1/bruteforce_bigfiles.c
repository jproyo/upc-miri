#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#define ASIZE 256


int BF(char *x, int m, char *y, int n) { 
  char *yb,*ini; 
  /* Searching */ 
  long int found=0;
  ini=y;
  for (yb = y; y <= ini+n-m; ++y) 
    if (memcmp(x, y, m) == 0) found++; 
  //printf("j=%d \n",y - yb);
  return found;
}


int main(int argc, char *argv[])
{

  FILE *f;
  char pattern[256];
  char filename[256];
  char *text,*intertext;
  long int  i,j,l,n1,n,lbuffer,lpattern;
  clock_t initemps;
  if (argc==1){
    printf("    1:: file\n");
    printf("    2:: pattern\n");
    printf("    3::length of the buffer\n");
    exit(-1);
  }
  lbuffer=atoi(argv[3]);
  
  sprintf(pattern,"%s",argv[2]);
  lpattern=strlen(pattern);
  sprintf(filename,"%s",argv[1]);
  printf("pattern: %s \n",pattern);
  text = (char *) malloc ((lbuffer+1) *sizeof(char));
  intertext = (char *) malloc ((2*lpattern) *sizeof(char));
  if (!(f = fopen(filename, "r"))) {
    fprintf (stderr, "I can't open the file: %s\n", filename);
    exit(1);
  }
  initemps=clock();
  n=0;n1=0;
  l=fread(text,1,lbuffer,f); 
  //fgets(text,lbuffer,f);
  while (!feof(f)){
    j=0;for (i=lbuffer-lpattern+1;i<l;i++){intertext[j]=text[i];j++;}
    n+=BF(pattern,lpattern,text,l);
    l=fread(text,1,lbuffer,f);
    for (i=0;i<lpattern-1;i++){intertext[j]=text[i];j++;}intertext[j]='\0';
    n1+=BF(pattern,lpattern,intertext,2*lpattern-2);
  }
  n+=BF(pattern,strlen(pattern),text,l);
  printf("Total=%ld\n",n+n1);
  printf( "Time: %f s\n", (clock()-initemps)/(double)CLOCKS_PER_SEC );
  fclose(f);
  free(text);
  return 0;
}
