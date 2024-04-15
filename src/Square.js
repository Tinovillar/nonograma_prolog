import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={`border ${value === '#' ? "bg-black" : "bg-white"} w-10 h-10 rounded`} onClick={onClick}>
            {value === 'X' ? value : null}
        </button>
    );
}

export default Square;