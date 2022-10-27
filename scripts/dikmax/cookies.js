export default function initCookies() {
  const cc = initCookieConsent();
  cc.run({
    current_lang: 'ru',
    page_scripts: true,
    cookie_expiration: 182,
    languages: {
      'ru': {
        consent_modal: {
          title: 'Переходи на тёмную стороу, у нас есть 🍪',
          description: "Блог использует Google Analytics для статистики, чтобы понимать, кто его читает. " +
            "Поэтому мне нужно ваше разрешение на использование соотвествующих куков. " +
            "<a href=\"/privacy/\" class=\"cc-link\">Политика конфиденциальности (на английском)</a>.",
          primary_btn: {
            text: 'Согласен',
            role: 'accept_all'      //'accept_selected' or 'accept_all'
          },
          secondary_btn: {
            text: 'Настройки',
            role: 'settings'       //'settings' or 'accept_necessary'
          },
        },
        settings_modal: {
          title: 'Настройки',
          save_settings_btn: 'Сохранить',
          accept_all_btn: 'Принять все',
          reject_all_btn: 'Отклонить все',
          close_btn_label: 'Закрыть',
          blocks: [
            {
              title: 'Аналитика',
              description: 'С помощью этих куков собирается информация, как вы пользуетесь сайтом, какие страницы ' +
                'посещаете и какие ссылки нажимаете. Все данные обезличены и не могут быть использованы для ' +
                'вашей идентификации.',
              toggle: {
                value: 'analytics',
                enabled: false,
                readonly: false
              },
            }, {
              title: 'Остались вопросы?',
              description: 'Посмотрите <a href="/privacy/" class="cc-link">политику конфиденциальности</a> или <a class="cc-link" href="mailto:me@dikmax.name?subject=Cookies">напишите мне</a>.',
            }
          ]
        }
      }
    }
  });
}
