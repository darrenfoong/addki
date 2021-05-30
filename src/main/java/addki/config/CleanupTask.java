package addki.config;

import addki.service.EntryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@EnableScheduling
@Configuration
public class CleanupTask {
  @Autowired private EntryService entryService;

  @Scheduled(fixedDelayString = "${cleanup.delay}")
  public void cleanup() {
    entryService.cleanup();
  }
}
