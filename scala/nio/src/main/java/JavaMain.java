import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousFileChannel;
import java.nio.channels.CompletionHandler;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

public class JavaMain {
  public static void main(String[] args) throws IOException, InterruptedException, ExecutionException {
    System.out.println("Start to learn IO!");

    List<String> stooges = Arrays.asList("Larry", "Moe", "Curly");
    stooges.stream().forEach(System.out::println);

    Path f = Paths.get("/Users/daewon/code/til/scala/nio/src/main/java/JavaMain.java");
    CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> "return value");
    CompletableFuture with = future.thenAccept(s -> System.out.println("Accept previous:: " + s));

    System.out.println(with.get());

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

    Thread.sleep(100);
  }
}
