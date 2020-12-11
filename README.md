# project_samara

Выполняли работу: 
Семина Ирина, Салий Сергей

Порядок обработки данных:
* Выгрузка ссылок на объявления со страниц поиска по продаже квартир
* Сбор информации по объекту с html страницы объявления (requests, re,bs4 (BeautifulSoup),lxml)
* Итог: около 5 тысяч наблюдений (50% от полной базы)
* Около 20 исходных характеристик

Описание файлов:
* Сбор первичных данных и обработка.ipynb - основная книжка питон с кодом парсера и обработки данных + визуальный анализ
* Проект_прога_моделирование.R - блокнот R с добавлением пары переменных и построением моделей со всеми сопутствующими расчетами
* Пробы с деревьями.ipynb - немного про попытки построить дерево для валидации полученных результатов по МНК
* CIAN_data_hrefs_v2.csv - Файл со скачанными ссылками на первом этапе (уже без дублей)
* CIAN_data_5k.csv базовые данные по отдельным объявлениям без особой очистики
* CIAN_data_full_farsh_red.csv- файл с данными после преобразования, кодирования категориальных переменных, убраны ненужные столбцы
* CIAN_data_for_lin.csv - файл на основе предыдущего для работы в R
