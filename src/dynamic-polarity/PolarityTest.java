import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.util.StringUtils;
import edu.stanford.nlp.naturalli.Polarity;
import edu.stanford.nlp.naturalli.NaturalLogicAnnotations;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.Scanner;

public class PolarityTest {

 public static final StanfordCoreNLP pipeline = new StanfordCoreNLP(new Properties(){{
    setProperty("annotators", "tokenize,ssplit,pos,lemma,parse,natlog"); 
    setProperty("ssplit.isOneSentence", "true");
    setProperty("tokenize.class", "PTBTokenizer");
    setProperty("tokenize.language", "en");
    setProperty("enforceRequirements", "false");
    setProperty("natlog.neQuantifiers", "true");
  }});

  public static Polarity[] annotate(String text) {
    Annotation ann = new Annotation(text);
    pipeline.annotate(ann);
    List<CoreLabel> tokens = ann.get(CoreAnnotations.SentencesAnnotation.class).get(0).get(CoreAnnotations.TokensAnnotation.class);
    Polarity[] polarities = new Polarity[tokens.size()];
    for (int i = 0; i < tokens.size(); ++i) {
      polarities[i] = tokens.get(i).get(NaturalLogicAnnotations.PolarityAnnotation.class);
    }
    return polarities;
  }

	public static void main(String[] args) {
    Scanner snr = new Scanner(System.in);
    while (true) {
      System.out.println("Write sentence for annotation ('q' to quit): ");
      String str = snr.nextLine();

      if (str.trim().equals("q")) {
        return;
      }

      Polarity[] polarities = annotate(str);
      String result = StringUtils.join(Arrays.stream(polarities).map(Polarity::toString).collect(Collectors.toList()), " ");
      System.out.println(result);
    }
	}
}

