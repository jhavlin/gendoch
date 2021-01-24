import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import ExcelJS, { Workbook } from 'exceljs';

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

const COLOR_1 = "AA88CC";
const COLOR_2 = "CC44AA";

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

const strToFileName = (str) => {
  const full = str.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
  const words = full.split(/\s+/)
  const res = words.length == 2
    ? words[1].replaceAll(/\W+/g, "")
    : full.replaceAll(/\s+/g, "_").replaceAll(/\W+/g, "");
  return res.toLowerCase();
};

const makeBorder = (ws, topRow, leftCol, rightCol, bottomRow, style = 'thin', background) => {
  for (let r = topRow; r <= bottomRow; r++) {
    for (let c = leftCol; c <= rightCol; c++) {
      const cell = ws.getRow(r).getCell(c);
      if (background) {
        cell.fill = {
          type: "pattern",
          pattern: "solid",
          fgColor: { argb: `FF${background}` }
        };
      }
      const border = {};
      if (r === topRow) {
        border.top = { style };
      }
      if (r === bottomRow) {
        border.bottom = { style };
      }
      if (c == leftCol) {
        border.left = { style };
      }
      if (c == rightCol) {
        border.right = { style };
      }
      cell.style.border = Object.assign({}, cell.style.border, border);
    }
  }
}

/**
 * Add worksheet for specified month info to a workbook.
 *
 * @param {Workbook} wb
 * @param {object} monthInfo
 * @param {object} data
 */
const addSheet = async (wb, monthInfo, data) => {
  const ws = wb.addWorksheet(monthInfo.monthName);
  ws.mergeCells("B3:D3"); // header
  ws.mergeCells("E3:F3"); // name
  ws.mergeCells("E4:F4"); // organization
  ws.mergeCells("E5:F5"); // empty
  ws.mergeCells("E6:F6"); // work time

  ws.getColumn(1).width = 5;
  ws.getColumn(2).width = 12;
  ws.getColumn(3).width = 12;
  ws.getColumn(4).width = 12;
  ws.getColumn(5).width = 24;
  ws.getColumn(6).width = 24;

  const titleRow = ws.getRow(3);
  titleRow.height = 18;
  const titleCell = ws.getCell("B3");
  titleCell.value = "Evidence docházky";
  titleCell.font = {
    name: "Calibri",
    size: 14,
    bold: true,
  };
  titleCell.alignment = { horizontal: "center" }

  ws.getCell("E3").value = `Jméno: ${data.name}`;
  ws.getCell("E4").value = `${data.organization}`;
  ws.getCell("E6").value = `Prac. doba od ${data.from} do ${data.to}`;
  makeBorder(ws, 3, 2, 6, 6, 'thin', COLOR_1);

  ws.getCell("B7").value = "Den";
  ws.getCell("C7").value = "Příchod";
  ws.getCell("D7").value = "Odchod";
  ws.getCell("E7").value = "Mimo pracoviště";
  ws.getCell("F7").value = "Důvod";
  makeBorder(ws, 7, 2, 6, 7, 'thin', COLOR_2);
  const headerRow = ws.getRow(7);
  for (let c = 2; c <= 6; c++) {
    const cell = headerRow.getCell(c);
    cell.style.border = Object.assign({}, cell.style.border, { bottom: { style: "thick" } });
    cell.alignment = { horizontal: "center" };
  }

  monthInfo.dayInfos.forEach((dayInfo, index) => {
    const row = ws.getRow(8 + index);
    row.height = 16;
    // date
    row.getCell(2).value = new Date(`${dayInfo.year}-${dayInfo.month}-${dayInfo.day}`);
    if (dayInfo.isActive && !dayInfo.isHoliday) {
      // from
      row.getCell(3).value = data.from;
      // to
      row.getCell(4).value = data.to;
    }
      // out of office
    // manually entered
    // explanation
    if (dayInfo.isHoliday && !dayInfo.isWeekend) {
      row.getCell(6).value = 'Státní svátek';
    }
    for (let c = 2; c <= 6; c++) {
      const cell = row.getCell(c);
      cell.style.border = {
        top: { style: "thin" },
        right: { style: "thin" },
        bottom: { style: "thin" },
        left: { style: "thin" },
      };
      if (c > 2) {
        cell.alignment = { horizontal: "center" }
      }
      if (dayInfo.isWeekend || dayInfo.isHoliday) {
        cell.fill = {
          type: "pattern",
          pattern: "solid",
          fgColor: { argb: `FF${COLOR_1}` }
        };
      }
    }
  });
  const footerStartRow = 8 + monthInfo.dayInfos.length;
  makeBorder(ws, footerStartRow, 2, 6, footerStartRow + 2);
  ws.mergeCells(footerStartRow + 1, 2, footerStartRow + 1, 4);
  ws.getRow(footerStartRow + 1).getCell(2).value = "Podpis pracovníka:";
  ws.getRow(footerStartRow + 1).getCell(6).value = "Kontroloval:";
};

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
  message.monthsInfo.forEach(monthInfo => addSheet(workbook, monthInfo, message));
  const buffer = await workbook.xlsx.writeBuffer();
  var blob = new Blob([buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'});
  var link = document.createElement('a');
  link.href = window.URL.createObjectURL(blob);
  var fileName = `${strToFileName(message.name)}_${message.year}.xlsx`;
  link.download = fileName;
  console.log(message);
  link.click();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
