const { toMatchImageSnapshot } = require('jest-image-snapshot');
const puppeteer = require('puppeteer');
expect.extend({ toMatchImageSnapshot });


it('CreateReactApp home', async () => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    await page.goto('http://localhost:8000/dist/index.html');
    await page.mouse.move(100, 100);
    await page.mouse.down();
    await page.mouse.move(100, 200);
    await page.mouse.move(200, 200);
    await page.mouse.move(200, 100);
    await page.mouse.move(100, 100);
    await page.mouse.up();
    const image = await page.screenshot();

    expect(image).toMatchImageSnapshot();
})
