import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import ExcelJS from 'exceljs';

const PUBLIC_HOLIDAYS_KEY = 'PUBLIC_HOLIDAYS';

const DEFAULT_HOLIDAYS = [
  { day: 1, month: 1 },
  { day: 1, month: 5 },
  { day: 8, month: 5 },
  { day: 5, month: 7 },
  { day: 6, month: 7 },
  { day: 28, month: 9 },
  { day: 28, month: 10 },
  { day: 17, month: 11 },
  { day: 24, month: 12 },
  { day: 25, month: 12 },
  { day: 26, month: 12 },
];

const resetPublicHolidays = () => {
  localStorage.setItem(PUBLIC_HOLIDAYS_KEY, JSON.stringify(DEFAULT_HOLIDAYS));
};

const setPublicHolidays = (holidays) => {
  localStorage.setItem(PUBLIC_HOLIDAYS_KEY, holidays);
};

const attempt = (fn, fallback) => {
  try {
    return fn();
  } catch (e) {
    return fallback;
  }
}

const customizedHolidaysStr = localStorage.getItem(PUBLIC_HOLIDAYS_KEY);
const holidays = customizedHolidaysStr
  ? attempt(() => JSON.parse(customizedHolidaysStr), DEFAULT_HOLIDAYS)
  : DEFAULT_HOLIDAYS;

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { holidays, year: new Date().getFullYear() }
});

app.ports.generate.subscribe(async (message) => {
  const workbook = new ExcelJS.Workbook();
  const buffer = await workbook.xlsx.writeBuffer();
  var blob = new Blob([buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'});
  var link = document.createElement('a');
  link.href = window.URL.createObjectURL(blob);
  var fileName = 'generated.xlsx';
  link.download = fileName;
  link.click();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
