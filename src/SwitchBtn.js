import React from 'react';

function SwitchBtn({painting, onClick}) {
    return (
        <button className={`w-36 h-20 ${painting ? "bg-black" : "bg-white"} rounded border-4 border-black`} onClick={onClick}> X
        </button>
    );
}

export default SwitchBtn;