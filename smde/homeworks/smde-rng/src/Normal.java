import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Normal {

    private FibLagged lagged = new FibLagged();

    public Normal(){
        lagged.init();
    }

    double random2(){
        double a=lagged.genrand();
        double b=lagged.genrand();
        if (a>0.5) b=b*(-1);
        return b;
    }


    double normal( double mu, double sigma ) {
        assert( sigma > 0. );
        double p, p1, p2;
        do {
            p1 = random2();
            p2 = random2();
            p = p1 * p1 + p2 * p2;
        } while ( p >= 1. );
        return mu + sigma * p1 * Math.sqrt( -2. * Math.log( p ) / p );
    }

    public static void main(String[] args) {
        Normal normal = new Normal();
        double mu = 10;
        double sigma = 1;
        StringBuffer buff = new StringBuffer();
        for(int i=0;i<1500;i++) {
            buff.append(normal.normal(mu, sigma));
            buff.append("\n");
        }
        try {
            Files.write(Paths.get("./1500_normal_mu_10_sigma_1.csv"), buff.toString().getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
