import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class mainTest {

    
    @Test
    public void test_displayMessage_1() {
        Main main = new Main();
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));
        main.displayMessage();
        assertEquals("Hello, World!\n", outContent.toString());
    }

   
    @Test
    public void test_main_1() {
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));

        String[] args = {};
        Main.main(args);

        String expectedOutput = "Hello, World!\n" +
                                "Number is greater than 40\n" +
                                "Numbers in array:\n" +
                                "1\n2\n3\n4\n5\n" +
                                "Using enhanced for loop:\n" +
                                "1\n2\n3\n4\n5\n";

        assert(outContent.toString().equals(expectedOutput));

        System.setOut(System.out);
    }

   
    @Test
    public void test_main_2() {
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        System.setOut(new PrintStream(outContent));

        Main.main(new String[]{});

        String expectedOutput = "Hello, World!\n" +
                                "Number is less than or equal to 40\n" +
                                "Numbers in array:\n" +
                                "1\n2\n3\n4\n5\n" +
                                "Using enhanced for loop:\n" +
                                "1\n2\n3\n4\n5\n";

        assert(outContent.toString().equals(expectedOutput));

        System.setOut(System.out);
    }

}
