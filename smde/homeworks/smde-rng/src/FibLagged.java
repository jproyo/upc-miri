import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class FibLagged {
    public int n=3000;
    public double antics[] = new double[30000];

    public void init() {
        for(int i=0;i<6000;i++)
            antics[i]=i;
    }

    public double genrand() {
        double a,b;

        double j = 4;
        double k = 400;
        double m = 2147483647;

        a=antics[n-(int)j];
        b=antics[n-(int)k];

        antics[n]=((a*b)%m);

        n++;
        return antics[n-1]/m;
    }

    public static void main(String[] args) {
        FibLagged lagged = new FibLagged();
        lagged.init();
        StringBuffer buff = new StringBuffer();
        for(int i=0;i<500;i++) {
            buff.append(lagged.genrand());
            buff.append("\n");
        }
        try {
            Files.write(Paths.get("./sample.csv"), buff.toString().getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}


