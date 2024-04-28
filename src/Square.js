import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={`border-2 ${value === '#' ? "bg-black" : "bg-white"} rounded-md`} onClick={onClick}>
            {value === 'X' ? value : null}
        </button>
    );
}

export default Square;