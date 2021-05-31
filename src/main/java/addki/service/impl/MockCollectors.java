package addki.service.impl;

import addki.request.CollectEntryRequest;
import addki.response.CollectEntryResponse;
import java.util.List;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class MockCollectors {
  @Autowired private RabbitTemplate rabbitTemplate;

  @Value("${mq.response}")
  private String responsePrefix;

  @RabbitListener(queues = "request.ko")
  public void handleKoRequest(CollectEntryRequest collectEntryRequest) {
    try {
      Thread.sleep(5000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("to study");
    collectEntryResponse.setAlternateForm("工夫");
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation(null);
    collectEntryResponse.setContexts(List.of("저는 매일 공부합니다"));
    collectEntryResponse.setTags(List.of("v"));

    rabbitTemplate.convertAndSend(responsePrefix, collectEntryResponse);
  }

  @RabbitListener(queues = "request.ja")
  public void handleJpRequest(CollectEntryRequest collectEntryRequest) {
    try {
      Thread.sleep(5000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("공부하다");
    collectEntryResponse.setAlternateForm("べんきょうする");
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation(null);
    collectEntryResponse.setContexts(null);
    collectEntryResponse.setTags(List.of("v"));
    rabbitTemplate.convertAndSend(responsePrefix, collectEntryResponse);
  }

  @RabbitListener(queues = "request.zh")
  public void handleZhRequest(CollectEntryRequest collectEntryRequest) {
    try {
      Thread.sleep(5000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("to study");
    collectEntryResponse.setAlternateForm(null);
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation("du2 shu1");
    collectEntryResponse.setContexts(null);
    collectEntryResponse.setTags(List.of("v"));

    rabbitTemplate.convertAndSend(responsePrefix, collectEntryResponse);
  }
}
