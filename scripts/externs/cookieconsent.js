/**
 * @returns {CookieConsent}
 */
function initCookieConsent() {}

/**
 * @typedef {{
 *   text: string,
 *   role: string
 * }}
 */
let CookieConsentButton;

/**
 * @typedef {{
 *   title: string,
 *   description: string,
 *   toggle: {
 *     value: string,
 *     enabled: boolean,
 *     readonly: boolean
 *   }
 * }}
 */
let CookieConsentBlock;

/**
 * @typedef {{
 *   consent_modal: {
 *     title: string,
 *     description: string,
 *     primary_btn: !CookieConsentButton,
 *     secondary_btn: !CookieConsentButton,
 *     revision_message: string
 *   },
 *   settings_modal: {
 *     title: string,
 *     save_settings_btn: string,
 *     accept_all_btn: string,
 *     reject_all_btn: string,
 *     close_btn_label: string,
 *     cookie_table_headers: Array<{
 *       col1: ?string,
 *       col2: ?string,
 *       col3: ?string
 *     }>,
 *     blocks: Array<CookieConsentBlock>
 *   }
 * }}
 */
let CookieConsentLanguage;

/**
 * @typedef {{
 *  current_lang: ?string,
 *  page_scripts: ?boolean,
 *  cookie_expiration: ?number,
 *  languages: ?Object<string, CookieConsentLanguage>
 * }}
 */
let CookieConsentOptions;

class CookieConsent {
  /**
   * @param {!CookieConsentOptions} options
   */
  run(options) {

  }
}