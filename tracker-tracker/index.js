import puppeteer from 'puppeteer';

const url = process.argv[2];

if (!url) {
  throw new Error('usage: node ./index.js URL');
}

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.setRequestInterception(true);
  page.on('request', (interceptedRequest) => {
    console.log(interceptedRequest.url());
    interceptedRequest.continue({}, 0);
  });

  await page.goto(url);
  await browser.close();
})();
