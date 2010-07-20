Erlyvideo vkontakte
===================

Библиотека может делать API вызовы к vkontakte.ru

* [Документация API вконтакте](http://vkontakte.ru/page2369267)
* [IFrame приложения](http://vkontakte.ru/page9279356)
* [Стандартная подпись API](http://vkontakte.ru/pages.php?o=-1&p=%C2%E7%E0%E8%EC%EE%E4%E5%E9%F1%F2%E2%E8%E5%20%EF%F0%E8%EB%EE%E6%E5%ED%E8%FF%20%F1%20API)
* [Защищенная подпись API](http://vkontakte.ru/pages.php?o=-1&p=%C7%E0%F9%E8%F9%E5%ED%ED%EE%E5%20%E2%E7%E0%E8%EC%EE%E4%E5%E9%F1%F2%E2%E8%E5%20%EF%F0%E8%EB%EE%E6%E5%ED%E8%FF%20%F1%20API)

Сейчас библиотека может:

* Делать единичные вызовы в vkontakte
* Сама определять, что вызов secure. и менять схему подписи
* Распаковывать результат с помощью mochijson2 в объекты


В ближайшее время сможет:

* Держать keepalive-соединение с вконтактом
* Лимитировать количество одновременных подключений к вконтакту
* Лимитировать количество запросов в секунду в вконтакту


Пример использования
====================

Не забудьте скопировать priv/vkontakte.conf.sample либо в папку priv вашего приложения, либо в /etc/erlyvideo/

<code>
vkontakte:start(),
{ok, Reply} = vkontakte:call("audio.get", [{uid,419158},{viewer_id,419158},{test_mode,1}]),
io:format("~p~n", [Reply]).
</code>