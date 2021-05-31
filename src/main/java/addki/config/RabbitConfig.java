package addki.config;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.core.TopicExchange;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RabbitConfig {
  private String topicExchangeName = "addki";

  private String koRequestQueue = "request.ko";
  private String jpRequestQueue = "request.jp";
  private String zhRequestQueue = "request.zh";
  private String responseQueue = "response";

  @Bean
  public Queue koRequestQueue() {
    return new Queue(koRequestQueue, false);
  }

  @Bean
  public Queue jpRequestQueue() {
    return new Queue(jpRequestQueue, false);
  }

  @Bean
  public Queue zhRequestQueue() {
    return new Queue(zhRequestQueue, false);
  }

  @Bean
  public Queue responseQueue() {
    return new Queue(responseQueue, false);
  }

  @Bean
  public TopicExchange exchange() {
    return new TopicExchange(topicExchangeName);
  }

  @Bean
  public Binding koRequestBinding(TopicExchange exchange) {
    return BindingBuilder.bind(koRequestQueue()).to(exchange).with(koRequestQueue().getName());
  }

  @Bean
  public Binding jpRequestBinding(TopicExchange exchange) {
    return BindingBuilder.bind(jpRequestQueue()).to(exchange).with(jpRequestQueue().getName());
  }

  @Bean
  public Binding zhRequestBinding(TopicExchange exchange) {
    return BindingBuilder.bind(zhRequestQueue()).to(exchange).with(zhRequestQueue().getName());
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
