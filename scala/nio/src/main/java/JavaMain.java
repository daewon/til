import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousFileChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.WritePendingException;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class JavaMain {
  public static void main(String[] args) throws IOException, InterruptedException {
    System.out.println("Start to learn IO!");

    List<String> stooges = Arrays.asList("Larry", "Moe", "Curly");
    stooges.stream().forEach(System.out::println);

    Path f = Paths.get("/Users/daewon/code/til/scala/nio/src/main/java/JavaMain.java");

    AsynchronousFileChannel ch = AsynchronousFileChannel.open(f.toAbsolutePath(), StandardOpenOption.READ);
    ByteBuffer buf = ByteBuffer.allocateDirect(2048);

    ch.read(buf, 0, null, new CompletionHandler<Integer, Object>() {
      @Override
      public void completed(Integer result, Object attachment) {
        buf.flip();
        System.out.println(result);
      }

      @Override
      public void failed(Throwable exc, Object attachment) {
      }
    });
  }
}
