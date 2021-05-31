package addki.config;

import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.AmqpAdmin;
import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.core.TopicExchange;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitAdmin;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Slf4j
@Configuration
public class RabbitConfig {
  @Value("${mq.topic-exchange}")
  private String topicExchangeName;

  @Value("${mq.request}")
  private String requestPrefix;

  @Value("${mq.response}")
  private String responsePrefix;

  @Value("${languages.supported}")
  private Set<String> supportedLanguages;

  @Bean
  public AmqpAdmin amqpAdmin(ConnectionFactory connectionFactory) {
    return new RabbitAdmin(connectionFactory);
  }

  @Bean
  public TopicExchange exchange() {
    return new TopicExchange(topicExchangeName);
  }

  @Bean
  public Queue responseQueue() {
    return new Queue(responsePrefix, false);
  }

  @Bean
  public Binding responseBinding(TopicExchange exchange) {
    return BindingBuilder.bind(responseQueue()).to(exchange).with(responseQueue().getName());
  }

  @Bean
  public RabbitTemplate rabbitTemplate(ConnectionFactory connectionFactory) {
    RabbitTemplate rabbitTemplate = new RabbitTemplate(connectionFactory);
    rabbitTemplate.setMessageConverter(jsonMessageConverter());
    return rabbitTemplate;
  }

  @Bean
  public MessageConverter jsonMessageConverter() {
    return new Jackson2JsonMessageConverter();
  }
}
